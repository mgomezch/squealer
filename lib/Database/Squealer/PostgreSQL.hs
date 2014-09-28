{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , NamedFieldPuns
  , NoImplicitPrelude
  , NoMonomorphismRestriction
  , NoMonoLocalBinds
  , OverloadedStrings
  , QuasiQuotes
  , RecordWildCards
  , TemplateHaskell
  , TypeFamilies
  , TypeSynonymInstances
  , UnicodeSyntax
  #-}

module Database.Squealer.PostgreSQL
  ( Document(Document, name, contents)
  , ToSqlString(toSqlString)
  , toSQL
  )
where

import Control.Applicative    (pure)
import Control.Lens.Fold      (has)
import Control.Monad          (join, liftM2)
import Control.Monad.Unicode  ((=≪), (≫=))
import Control.Newtype        (unpack)
import Data.Bool              (Bool)
import Data.Eq.Unicode        ((≡))
import Data.Function          (($), const, flip)
import Data.Function.Unicode  ((∘))
import Data.Functor           ((<$), (<$>), fmap)
import Data.Graph             (graphFromEdges, topSort)
import Data.List              (filter, intersperse, null, reverse)
import Data.Maybe             (maybeToList)
import Data.Monoid            ((<>), mappend, mconcat)
import Data.Monoid.Unicode    ((⊕))
import Data.String            (String)
import Data.Text              (Text, replace)
import Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import Data.Text.Lazy         (toStrict)
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Tuple.HT          (fst3, uncurry3)
import Prelude                (error)
import Text.Shakespeare.Text  (text, textFile, toText)
import Text.ShellEscape       (bash, bytes)
import Text.Show              (Show, show)

import qualified Data.Char as C (toLower)
import qualified Data.Text as T (pack)

import Database.Squealer.Types (_Reference, Database(Database, dbname, epilogue, prologue, tables), Identifier(Identifier), Table(Table, tablename, key, columns), Column(Attribute, Reference, colname, coltype, onUpdate, onDelete, target), Variance(Cascade, Preserve, Restrict, SetNull))



class ToSqlString a where
  toSqlString ∷ a → Builder

instance ToSqlString Text where
  toSqlString = toText ∘ (\t → mconcat ["'", replace "'" "''" t, "'"])

instance ToSqlString String where
  toSqlString = toSqlString ∘ T.pack

instance ToSqlString Identifier where
  toSqlString = toSqlString ∘ unpack



data Document
  = Document
    { name     ∷ Text
    , contents ∷ Text
    }

{- Note:
Despite what may appear to be incomplete factoring in this type, a
plain document piece is intended to be distinguished from a folded
document piece with zero children and the same content piece.

Folded document pieces are always rendered with enclosing fold markers,
even if they have no children, while plain document pieces are never
rendered enclosed in fold markers.
-}
data DocumentPiece
  = Plain
    { contentPiece ∷ Builder
    }
  | Folded
    { contentPiece ∷ Builder
    , children ∷ [DocumentPiece]
    }

renderDocumentPiece ∷ DocumentPiece → Text
renderDocumentPiece d
  = toStrict ∘ toLazyText $ renderDocBit d ⊕ "\n"
  where
    renderDocBit ∷ DocumentPiece → Builder
    renderDocBit d' = case d' of
      Plain {..} → contentPiece
      Folded {..} → mconcat
        [ ifHasChildren o, o, contentPiece, "\n"
        , c, ifHasChildren "\n"
        , mconcat $ intersperse "\n" $ renderDocBit <$> children
        , ifHasChildren c
        ]
        where
          o = "/*{{{*/"
          c = "/*}}}*/"
          ifHasChildren t = if null children then "" else t



data Event = Update | Delete deriving Show

instance ToSqlString Event where
  toSqlString = toSqlString ∘ fmap C.toLower ∘ show



toSQL ∷ Database → [Document]
toSQL
  = generateDatabase
  where
    plainTemplate = Plain ∘ ($ ())
    foldedTemplate = (`Folded` []) ∘ ($ ())
    titled title = Folded $ mconcat ["/* ", title, " */"]

    cond ∷ Bool → a → a → a
    cond c t f = if c then t else f

    withSurrogate keyColumns
      = if null keyColumns
        then [Attribute "identity" "uuid"]
        else keyColumns

    keyColname column = case column of
      Attribute {..} → colname
      Reference {..} → colname ⊕ " version"


    generateDatabase Database {..}
      = pure generateLoader
      ⊕ pure generatePrelude
      ⊕ (simpleDocument "prologue.sql" <$> maybeToList prologue)
      ⊕ (simpleDocument "epilogue.sql" <$> maybeToList epilogue)
      ⊕ fmap generateTable tables
      where
        simpleDocument name contents = Document {..}

        generateLoader
          = Document
            { name = unpack dbname ⊕ ".sh"
            , contents = toStrict ∘ toLazyText $ $(textFile "templates/load.sht") ()
            }
          where
            files
              = pure "prelude"
              ⊕ maybeToList ("prologue" <$ prologue)
              ⊕ topoSort tables
              ⊕ maybeToList ("epilogue" <$ epilogue)

            shellEscape
              = decodeUtf8
              ∘ bytes
              ∘ bash
              ∘ encodeUtf8

            topoSort
              = reverse
              ∘ fmap fst3
              ∘ uncurry3 ((const ∘) ∘ flip fmap ∘ topSort)
              ∘ graphFromEdges
              ∘ fmap tableToEdges
              where
                tableToEdges Table {..}
                  = join (,,) (unpack tablename)
                  ∘ fmap (unpack ∘ target)
                  ∘ filter (has _Reference)
                  $ key ⊕ columns

        generatePrelude
          = Document
            { name = "prelude.sql"
            , contents = toStrict ∘ toLazyText $ $(textFile "templates/prelude.sqlt") ()
            }

        referredKey target = withSurrogate $ key =≪ filter ((target ≡) ∘ tablename) tables

        generateTable Table {..}
          = Document
            { name = unpack tablename ⊕ ".sql"
            , contents =
              renderDocumentPiece $
                titled ([text|#{tablename} schema|] ()) -- FIXME: table name could contain «*/»!
                  [ foldedTemplate $(textFile "templates/schema.sqlt")
                  , titled "Row versioning backend"
                    [ identity
                    , titled "Row version journal"         [plainTemplate $(textFile "templates/backend/journal.sqlt"   )]
                    , titled "Row version revocation"      [plainTemplate $(textFile "templates/backend/revocation.sqlt")]
                    , titled "Row version succession"      [plainTemplate $(textFile "templates/backend/succession.sqlt")]
                    , titled "Active row version tracking" [plainTemplate $(textFile "templates/backend/active.sqlt"    )]
                    ]
                  , titled "Attributes" $ generateColumn <$> columns
                  , titled "Frontend"
                    [ titled "Version view"
                      [ version
                      ]
                    , titled "Transactional view"
                      [ view
                      , titled "Row version tracking triggers"
                        [ titled "Insert into view" viewInsert
                        , titled "Delete from view" viewDelete
                        , titled "Update view"      viewUpdate
                        ]
                      , titled "Column triggers" $ generateColumnTriggers <$> columns
                      , titled "Reference triggers" $ generateReferenceTriggers =≪ filter (has _Reference) (key ⊕ columns)
                      ]
                    ]
                  ]
            }
          where
            identity = titled "Row identification" [plainTemplate $(textFile "templates/backend/identity.sqlt")]
              where
                identityColumn column = case column of
                  Attribute {..} → $(textFile "templates/backend/identity/attribute.sqlt") ()
                  Reference {..} → $(textFile "templates/backend/identity/reference.sqlt") ()

                keyUnique = $(textFile "templates/backend/identity/keyunique.sqlt") ()

            generateColumn column
              = titled (toText $ colname column) -- FIXME: column name could contain «*/»!
              $ foldedTemplate <$> case column of
                Attribute {..} →
                  [ $(textFile "templates/backend/columns/attribute/state.sqlt")
                  , $(textFile "templates/backend/columns/attribute/proxy.sqlt")
                  ]
                Reference {..} →
                  [ $(textFile "templates/backend/columns/reference.sqlt")
                  ]

            (version, view)
              = ( plainTemplate $(textFile "templates/frontend/version.sqlt")
                , foldedTemplate $(textFile "templates/frontend/view.sqlt")
                )
              where
                keySelect Attribute {..} = $(textFile "templates/frontend/query/select/key/attribute.sqlt") ()
                keySelect Reference {..} = $(textFile "templates/frontend/query/select/key/reference.sqlt") ()
                  where
                    referenceKeyColumn keyColumn = $(textFile "templates/frontend/query/select/key/reference/referredkey.sqlt") ()

                normalSelect Attribute {..} = $(textFile "templates/frontend/query/select/columns/attribute.sqlt") ()
                normalSelect Reference {..} = $(textFile "templates/frontend/query/select/columns/reference.sqlt") ()
                  where
                    referenceKeyColumn keyColumn = $(textFile "templates/frontend/query/select/columns/reference/referredkey.sqlt") ()

                keyJoin column = case column of
                  Attribute {..} → $(textFile "templates/frontend/query/join/key/attribute.sqlt") ()
                  Reference {..} → $(textFile "templates/frontend/query/join/key/reference.sqlt") ()

                normalJoin column = case column of
                  Attribute {..} → $(textFile "templates/frontend/query/join/columns/attribute.sqlt") ()
                  Reference {..} → $(textFile "templates/frontend/query/join/columns/reference.sqlt") ()


            viewInsert
              = [ titled "Function" [function]
                , titled "Trigger"  [trigger]
                ]
              where
                function = plainTemplate $(textFile "templates/frontend/triggers/core/insert/function.sqlt")
                  where
                    surrogateNullCheck = $(textFile "templates/frontend/triggers/core/insert/function/surrogatenullcheck.sqlt") ()

                    referenceNullCheck column = $(textFile "templates/frontend/triggers/core/insert/function/referencenullcheck.sqlt") ()
                    referenceColumn    column = $(textFile "templates/frontend/triggers/core/insert/function/referencecolumn.sqlt"   ) ()
                    referenceNewColumn column = $(textFile "templates/frontend/triggers/core/insert/function/referencenewcolumn.sqlt") ()
                    referenceJoin      column = $(textFile "templates/frontend/triggers/core/insert/function/referencejoin.sqlt"     ) ()

                    identityJoin Attribute {..} = $(textFile "templates/frontend/triggers/core/insert/function/identityjoin/attribute.sqlt") ()
                    identityJoin Reference {..} = $(textFile "templates/frontend/triggers/core/insert/function/identityjoin/reference.sqlt") ()
                      where
                        referenceIdentityJoin keyColumn = $(textFile "templates/frontend/triggers/core/insert/function/identityjoin/reference/referredkey.sqlt") ()

                    loadReferredIdentity Reference {..} = $(textFile "templates/frontend/triggers/core/insert/function/loadreferredidentity.sqlt") ()
                      where
                        loadReferredIdentityJoin keyColumn = $(textFile "templates/frontend/triggers/core/insert/function/loadreferredidentity/join.sqlt") ()

                    surrogateInsertIdentity = $(textFile "templates/frontend/triggers/core/insert/function/insertidentity/surrogate.sqlt") ()
                    naturalInsertIdentity   = $(textFile "templates/frontend/triggers/core/insert/function/insertidentity/natural.sqlt") ()

                trigger = plainTemplate $(textFile "templates/frontend/triggers/core/insert/trigger.sqlt")


            viewDelete
              = [ titled "Function" [function]
                , titled "Trigger"  [trigger]
                ]
              where
                function = plainTemplate $(textFile "templates/frontend/triggers/core/delete/function.sqlt")
                  where
                    joinCondition column = $(textFile "templates/frontend/triggers/core/delete/function/joincondition.sqlt") ()

                trigger = plainTemplate $(textFile "templates/frontend/triggers/core/delete/trigger.sqlt")


            viewUpdate
              = [ titled "Function" [function]
                , titled "Trigger"  [trigger]
                ]
              where
                function = plainTemplate $(textFile "templates/frontend/triggers/core/update/function.sqlt")
                  where
                    keyColnames = key ≫= \ column → case column of
                      Attribute {..} → pure colname
                      Reference {..} → ((colname ⊕ " -> ") ⊕) ∘ keyColname <$> referredKey target

                    nullCheck colname = $(textFile "templates/frontend/triggers/core/update/function/nullcheck.sqlt") ()
                    surrogateNullCheck = $(textFile "templates/frontend/triggers/core/update/function/surrogatenullcheck.sqlt") ()

                    referredIdentity Reference {..} = $(textFile "templates/frontend/triggers/core/update/function/referredidentity.sqlt") ()
                      where
                        existingVersionCondition column = $(textFile "templates/frontend/triggers/core/update/function/referredidentity/existingversioncondition.sqlt") ()
                        referredIdentityJoin column = $(textFile "templates/frontend/triggers/core/update/function/referredidentity/join.sqlt") ()

                    joinActive      column = $(textFile "templates/frontend/triggers/core/update/function/joinactive.sqlt") ()
                    joinNewIdentity column = $(textFile "templates/frontend/triggers/core/update/function/joinnewidentity.sqlt") ()

                    surrogateInsertIdentity = $(textFile "templates/frontend/triggers/core/update/function/insertidentity/surrogate.sqlt") ()
                    naturalInsertIdentity   = $(textFile "templates/frontend/triggers/core/update/function/insertidentity/natural.sqlt") ()

                trigger = plainTemplate $(textFile "templates/frontend/triggers/core/update/trigger.sqlt")


            generateColumnTriggers = liftM2 titled (toText ∘ colname) columnTriggers -- FIXME: column name could contain «*/»!
              where
                columnTriggers Attribute {..}
                  = [ titled "Function" [plainTemplate $(textFile "templates/frontend/triggers/column/attribute/function.sqlt")]
                    , titled "Trigger"  [plainTemplate $(textFile "templates/frontend/triggers/column/attribute/trigger.sqlt")]
                    ]
                  where
                    identityJoin keyColumn = $(textFile "templates/frontend/triggers/column/attribute/function/identityjoin.sqlt") ()

                columnTriggers Reference {..}
                  = [ titled "Insert into view" insertTrigger
                    , titled "Update view"      updateTrigger
                    ]
                  where
                    insertTrigger = [function, trigger]
                      where
                        function = titled "Function" [plainTemplate $(textFile "templates/frontend/triggers/column/reference/insert/function.sqlt")]
                          where
                            targetNotNull keyColumn = $(textFile "templates/frontend/triggers/column/reference/insert/function/targetnotnull.sqlt") ()
                            identityJoin  keyColumn = $(textFile "templates/frontend/triggers/column/reference/insert/function/identityjoin.sqlt") ()
                            targetJoin    keyColumn = $(textFile "templates/frontend/triggers/column/reference/insert/function/targetjoin.sqlt") ()

                        trigger = titled "Trigger" [plainTemplate $(textFile "templates/frontend/triggers/column/reference/insert/trigger.sqlt")]

                    updateTrigger = [function, trigger]
                      where
                        function = titled "Function" [plainTemplate $(textFile "templates/frontend/triggers/column/reference/update/function.sqlt")]
                          where
                            identicalReferredKeyCondition keyColumn = $(textFile "templates/frontend/triggers/column/reference/update/function/identicalreferredkeycondition.sqlt") ()
                            referredKeyNullCheck          keyColumn = $(textFile "templates/frontend/triggers/column/reference/update/function/referredkeynullcheck.sqlt") ()
                            referredKeyChangeCondition    keyColumn = $(textFile "templates/frontend/triggers/column/reference/update/function/referredkeychangecondition.sqlt") ()
                            referredKeyNoChangeCondition  keyColumn = $(textFile "templates/frontend/triggers/column/reference/update/function/referredkeynochangecondition.sqlt") ()
                            identityJoin                  keyColumn = $(textFile "templates/frontend/triggers/column/reference/update/function/identityjoin.sqlt") ()
                            targetJoin                    keyColumn = $(textFile "templates/frontend/triggers/column/reference/update/function/targetjoin.sqlt") ()

                        trigger = titled "Trigger" [plainTemplate $(textFile "templates/frontend/triggers/column/reference/update/trigger.sqlt")]


            generateReferenceTriggers Reference {..}
              = generateReferenceTrigger Update onUpdate
              ⊕ generateReferenceTrigger Delete onDelete
              where
                generateReferenceTrigger event variance
                  = case variance of
                    Preserve → []
                    Cascade  → pure $ header cascade
                    Restrict → pure $ header restrict
                    SetNull  → error "TODO: generate triggers for SET NULL action on update or delete"
                  where
                    header = titled $ [text|#{C.toLower <$> show variance} on #{C.toLower <$> show event} to #{colname}|] ()
                    functionName = mconcat [Identifier ∘ T.pack $ C.toLower <$> show variance, " ", Identifier ∘ T.pack $ C.toLower <$> show event, " on ", tablename, " view ", colname, " reference"]
                    triggerName  = "20 " ⊕ functionName

                    eventTable ∷ Identifier
                    eventTable = case event of
                      Update → "succession"
                      Delete → "revocation"

                    cascade
                      = [ titled "Function" [plainTemplate cascadeTemplate]
                        , titled "Trigger"  [plainTemplate $(textFile "templates/frontend/triggers/reference/cascade/trigger.sqlt")]
                        ]
                      where
                        cascadeTemplate = case event of
                          Update → $(textFile "templates/frontend/triggers/reference/cascade/function/update.sqlt")
                          Delete → $(textFile "templates/frontend/triggers/reference/cascade/function/delete.sqlt")

                        setReferenceKeyName  keyColumn = $(textFile "templates/frontend/triggers/reference/cascade/function/update/setreferencekey/name.sqlt") ()
                        setReferenceKeyValue keyColumn = $(textFile "templates/frontend/triggers/reference/cascade/function/update/setreferencekey/value.sqlt") ()

                    restrict
                      = [ titled "Function" [plainTemplate $(textFile "templates/frontend/triggers/reference/restrict/function.sqlt")]
                        , titled "Trigger"  [plainTemplate $(textFile "templates/frontend/triggers/reference/restrict/trigger.sqlt")]
                        ]
