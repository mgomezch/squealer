{-# LANGUAGE
    NoImplicitPrelude
  , OverloadedStrings
  , RecordWildCards
  , UnicodeSyntax
  #-}

module Main (main) where

import Control.Monad                (forM_, liftM2)
import Control.Monad.Unicode        ((=≪))
import Control.Newtype              (unpack)
import Data.Bool                    (Bool(False))
import Data.ByteString.Char8        (readFile)
import Data.Either                  (either)
import Data.Function                (($))
import Data.Function.Unicode        ((∘))
import Data.Monoid.Unicode          ((⊕))
import Data.Text.IO                 (writeFile)
import Data.Yaml                    (decodeEither)
import Database.Squealer.PostgreSQL (contents, name, toSQL)
import Database.Squealer.Types      (Database(Database, dbname, tables))
import Prelude                      (error)
import System.Directory             (createDirectoryIfMissing)
import System.Environment           (getArgs)
import System.IO                    (IO)

import qualified Data.Text as T (unpack)

main ∷ IO ()
main
  = do
    [filename] ← getArgs
    either error writeDB ∘ decodeEither =≪ readFile filename
  where
    writeDB db @ Database {..}
      = do
        createDirectoryIfMissing False $ T.unpack prefix
        forM_ (toSQL db) $ liftM2 writeFile (T.unpack ∘ (prefix ⊕) ∘ name) contents
      where
        prefix = unpack dbname ⊕ "/"
