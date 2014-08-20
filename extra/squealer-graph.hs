{- FIXME:
For some reason, the output file is never created if this file is
compiled and run, but it works fine in GHCi.
-}

{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PackageImports     #-}
{-# LANGUAGE ParallelListComp   #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE UnicodeSyntax      #-}

import Control.Applicative                 (empty, pure)
import Control.Category.Unicode            ((∘))
import Control.Concurrent                  (forkIO)
import Control.Monad                       (guard, void)
import Control.Monad.Unicode               ((=≪), (≫))
import Data.Bool                           (Bool(False, True), not)
import Data.Either                         (either)
import Data.Eq.Unicode                     ((≠), (≡))
import Data.Foldable                       (find)
import Data.Function                       (($), id)
import Data.Functor                        ((<$>), fmap)
import Data.Graph.Inductive.NodeMap        (mkMapGraph)
import Data.Graph.Inductive.PatriciaTree   (Gr)
import Data.GraphViz                       (fmtEdge, fmtNode, graphToDot, nonClusteredParams, setDirectedness)
import Data.GraphViz.Attributes            (oDot, toLabel)
import Data.GraphViz.Attributes.Colors.SVG (SVGColor(Black, White))
import Data.GraphViz.Attributes.Complete   (Attribute(ArrowTail, Dir, HeadPort, Label, Shape), Color(SVGColor), DirType(Back), Label(StrLabel), PortName(PN), PortPos(LabelledPort), Shape(PlainText))
import Data.GraphViz.Attributes.HTML       (Align(HLeft), Attribute(Align, BGColor, Border, CellBorder, CellPadding, CellSpacing, ColSpan, Color, Face, Port), Cell(LabelCell, VerticalRule), Format(Bold), Label(Text), Row(Cells, HorizontalRule), TextItem(Font, Format, Str), tableAttrs, tableFontAttrs, tableRows)
import Data.GraphViz.Commands              (runGraphviz, GraphvizOutput(Svg))
import Data.List                           (intersperse, null)
import Data.List.Unicode                   ((∈))
import Data.Map                            (Map, fromList, lookup)
import Data.Maybe                          (maybe)
import Data.Monoid.Unicode                 ((⊕))
import Data.String                         (String)
import Data.Text                           (filter)
import Data.Text.Lazy                      (fromStrict, pack)
import Data.Tuple                          (fst, uncurry)
import Data.Yaml                           (decodeFileEither)
import Database.Squealer                   (Column(Attribute, Reference, colname, coltype, onDelete, onUpdate, target), Database(Database, tables), Identifier(Identifier, unIdentifier), Table(Table, columns, key, tablename), Variance(Preserve))
import Prelude                             (Integer)
import System.Environment                  (getArgs)
import System.IO                           (IO, print)
import Text.Show                           (show)

import qualified Data.GraphViz.Attributes.HTML as HTML (Table(HTable))

import Prelude (error)



type Edge = (Identifier, Identifier)



main ∷ IO ()
main
  = do
    [infile, outfile] ← getArgs
    either print (renderGraph outfile) =≪ decodeFileEither infile



renderGraph ∷ String → Database → IO ()
renderGraph
  outfile
  database @ Database {..}
  = view
  $ databaseToGraph database
  where
    portMap
      = fromList
      $ [ ((tablename, colname), n)
        | n ← [1..]
        | Table     {..} ← tables
        , Reference {..} ← key ⊕ columns
        ]

    view
      = void
      ∘ forkIO
      ∘ void
      ∘ (\ graph → runGraphviz graph Svg outfile)
      ∘ setDirectedness
        graphToDot
        nonClusteredParams
          { fmtEdge = fmtEdge'
          , fmtNode = fmtNode'
          }
      where
        fmtEdge' (_, _, edge@(tablename', colname'))
          = [ Label $ StrLabel ""
            , HeadPort $ LabelledPort edgePort empty
            , Dir Back
            ]
            ⊕ invariantArrow
          where
            edgePort
              = PN ∘ pack ∘ show
              ∘ maybe exception id
              $ lookup edge portMap
              where
                exception
                  = error
                  $ "edge not found in port map: "
                  ⊕ show edge

            invariantArrow
              = guard isInvariant
              ≫ [ArrowTail oDot]
              where
                isInvariant
                  = not
                  $ null
                    [ ()
                    | Table {..} ← tables
                    , tablename ≡ tablename'
                    , Reference {..} ← key ⊕ columns
                    , colname ≡ colname'
                    , Preserve ∈ [onUpdate, onDelete]
                    ]

        fmtNode' (_, l)
          = [ Shape PlainText
            , toLabel $ nodeRecord l
            ]
          where
            nodeRecord node
              = mkHTable portMap
              ∘ maybe exception id
              ∘ find ((node ≡) ∘ tablename)
              $ tables
              where
                exception
                  = error
                  $ "node not found in database: "
                  ⊕ show node


databaseToGraph ∷ Database → Gr Identifier Edge
databaseToGraph Database {..}
  = fst
  $ mkMapGraph
    (tablename <$> tables)
    [ (target, tablename, (tablename, colname))
    | Table     {..} ← tables
    , Reference {..} ← key ⊕ columns
    ]



mkHTable ∷ Map Edge Integer → Table → HTML.Table
mkHTable
  portMap
  Table {..}
  = HTML.HTable
    { tableFontAttrs
      = pure
        [ Face "sans-serif" 
        ]
    , tableAttrs
      = [ Border 1
        , CellBorder 0
        , CellPadding 4
        , CellSpacing 0
        , BGColor $ SVGColor Black
        ]
    , tableRows
      = header
      ⊕ (HorizontalRule : rows)
    }
  where
    header
      = [ Cells
          [ LabelCell
            headerAttributes
            ∘ Text
            ∘ pure ∘ Format Bold
            ∘ pure ∘ Font [Color $ SVGColor White]
            ∘ pure ∘ Str ∘ fromStrict ∘ unIdentifier
            $ tablename
          ]
        ]

    headerAttributes
      = [ Align HLeft
        , Border 0
        , CellPadding 8
        , ColSpan 2
        , BGColor $ SVGColor Black
        ]

    rows
      = intersperse HorizontalRule
      $ Cells
      ∘ uncurry toRow
      <$> allColumns
      where
        allColumns
          = (True , ) `fmap` key
          ⊕ (False, ) `fmap` columns

    labelWith bold attributes
      = LabelCell
        ( [ Align HLeft
          , BGColor $ SVGColor White
          ]
        ⊕ attributes
        )
      ∘ Text ∘ format ∘ pure ∘ Str ∘ fromStrict ∘ unIdentifier
      where
        format
          = if bold
            then pure ∘ Format Bold
            else id

    toRow ∷ Bool → Column → [Cell]
    toRow inKey Attribute {..}
      = [ labelWith inKey empty colname
        , VerticalRule
        , labelWith False empty
          ∘ Identifier
          ∘ filter ('"' ≠)
          $ coltype
        ]
    toRow inKey Reference {..}
      = [ labelWith inKey attributes colname
        ]
      where
        attributes
          = [ ColSpan 2
            , Port ∘ PN ∘ pack ∘ show
              ∘ maybe exception id
              $ lookup edge portMap
            ]
          where
            edge = (tablename, colname)
            exception
              = error
              $ "edge not found in port map: "
              ⊕ show edge
