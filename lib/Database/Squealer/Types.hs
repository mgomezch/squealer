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

module Database.Squealer.Types
  ( Identifier(Identifier, unIdentifier), escapeIdentifier
  , Database  (Database, dbname, epilogue, prologue, tables)
  , Table     (Table, tablename, key, columns)
  , Column    (Attribute, Reference, colname, coltype, target, onUpdate, onDelete), _Reference
  , Variance  (Cascade, Preserve, Restrict, SetNull)
  )
where

import Control.Applicative    (empty, pure)
import Control.Lens.Cons      (_head)
import Control.Lens.Setter    ((%~))
import Control.Lens.TH        (makeClassyFor, makePrisms)
import Control.Monad.Unicode  ((=≪))
import Control.Newtype        (Newtype, pack, unpack)
import Data.Aeson             (FromJSON, ToJSON, Value(String), parseJSON, toJSON)
import Data.Aeson.TH          (SumEncoding(ObjectWithSingleField), defaultOptions, deriveJSON, fieldLabelModifier, sumEncoding)
import Data.Char              (toUpper)
import Data.Eq                (Eq)
import Data.Function          (on)
import Data.Function.Unicode  ((∘))
import Data.Functor           (fmap)
import Data.Maybe             (Maybe, maybe)
import Data.Monoid            (Monoid, mappend, mconcat, mempty)
import Data.Ord               (Ord)
import Data.String            (IsString, fromString)
import Data.Text              (Text, replace, toLower)
import Text.Read              (Read, readMaybe)
import Text.Shakespeare.Text  (ToText, toText)
import Text.Show              (Show, show)

import qualified Data.Text as T (pack)



newtype Identifier
  = Identifier { unIdentifier ∷ Text }
  deriving (Eq, Ord, Read, Show)

instance Newtype Identifier Text where
  pack   = Identifier
  unpack = unIdentifier

instance Monoid Identifier where
  mempty  = pack mempty
  mappend = (pack ∘) ∘ mappend `on` unpack

{- TODO:
Use PostgreSQL Unicode escaped identifiers to escape question mark
characters using their Unicode code point number.  This should avoid
breaking postgresql-simple.  As of PostgreSQL 9.3.2, a bug in the
lexer breaks loading the Squealer-generated SQL source if these
identifiers are used.

escapeIdentifier ∷ Identifier → Text
escapeIdentifier
  = quote ∘ escape ∘ unpack
  where
    quote t = mconcat ["U&\"", t, "\""]
    escape
      = replace "\"" "\\0022"
      ∘ replace "?"  "\\003f"
      ∘ replace "\\" "\\005c"
-}
escapeIdentifier ∷ Identifier → Text
escapeIdentifier
  = (\ t → mconcat ["\"", replace "\"" "\"\"" t, "\""])
  ∘ unpack

instance ToText Identifier where
  toText = toText ∘ escapeIdentifier

instance IsString Identifier where
  fromString = Identifier ∘ T.pack

instance ToJSON Identifier where
  toJSON = String ∘ unpack

instance FromJSON Identifier where
  parseJSON = fmap Identifier ∘ parseJSON



data Variance
  = Preserve
  | Cascade
  | Restrict
  | SetNull
  deriving (Eq, Read, Show)

-- FIXME: this should allow `set null` as JSON syntax for `SetNull`.
instance ToJSON Variance where
  toJSON = String ∘ toLower ∘ T.pack ∘ show

instance FromJSON Variance where
  parseJSON
    = (maybe empty pure ∘ readMaybe ∘ (_head %~ toUpper) =≪) -- FIXME: this use of empty is partial and yields exceptions that simply say `empty`.
    ∘ parseJSON


{- Note:
The representation of a database should look more like this:

    data Database
      = Database
        { comment, prologue, epilogue ∷ Text
        , tables ∷ Map Identifier Table
        }

    data Table
      = Table
        { comment, prologue, epilogue ∷ Text
        , columns ∷ Map Identifier Column
        }

    data Column
      = Column
        { comment ∷ Text
        , columnInKey ∷ InKey
        , columnData ∷ ColumnData
        }

    data InKey
      = KeyComponent
      | NonKeyComponent

    data ColumnData
      = CAttribute { attribute ∷ Attribute }
      | CReference { reference ∷ Reference }

    data Attribute
      = Attribute
        { attributeType ∷ Text
        }

    data Reference
      = Reference
        { target ∷ Identifier
        , onUpdate, onDelete ∷ Variance
        }

If this representation was used, databases with repeated table names or
tables with repeated column names would be inexpressible.  This is, of
course, desirable.  Moreover, these types are Haskell98 and could be
manipulated with lenses and prisms with no partiality.  The only problem
is that using this representation would imply a nontrivial rewriting
effort for Squealer.  Also, the implied schema for input files is
incompatible with the currently used schema, which is directly derived
from the current type schema and hence allows for many errors.  If this
change is implemented, the old Squealer input file format would become
unsupported.

Aeson can probably still derive FromJSON/ToJSON for this schema, though.
-}

data Column
  = Attribute
    { colname ∷ Identifier
    , coltype ∷ Text
    }
  | Reference
    { colname  ∷ Identifier
    , target   ∷ Identifier
    , onUpdate ∷ Variance
    , onDelete ∷ Variance
    }
  deriving (Eq, Read, Show)

-- FIXME: `onUpdate` should default to `Cascade`, and `onDelete` to `Restrict`.
let
  fieldLabelModifier f = case f of
    "colname" → "name"
    "coltype" → "type"
    _ → f
  in deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField, fieldLabelModifier } ''Column

makeClassyFor "HasColumn" "_column" [("colname", "_colname")] ''Column
makePrisms ''Column


data Table
  = Table
    { tablename ∷ Identifier
    , key       ∷ [Column]
    , columns   ∷ [Column]
    }
  deriving (Eq, Read, Show)

let
  fieldLabelModifier f = case f of
    "tablename" → "table name"
    "key"       → "primary key columns"
    "columns"   → "data columns"
    _ → f
  in deriveJSON defaultOptions { fieldLabelModifier } ''Table


data Database
  = Database
    { dbname   ∷ Identifier
    , prologue ∷ Maybe Text
    , tables   ∷ [Table]
    , epilogue ∷ Maybe Text
    }
  deriving (Eq, Read, Show)

let
  fieldLabelModifier f = case f of
    "dbname" → "database name"
    _ → f
  in deriveJSON defaultOptions { fieldLabelModifier } ''Database
