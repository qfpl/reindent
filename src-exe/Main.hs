{-# LANGUAGE DataKinds #-}

module Main where

import Control.Lens
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Validation (Validation (Success, Failure))

import Language.Python.Parse as HPY
import Language.Python.Internal.Optics
import Language.Python.Internal.Render
import Language.Python.Internal.Syntax

import Reindent.FileIO
import Reindent.Options

---- Indentation stuff
setStatementIndents :: [Whitespace] -> Statement '[] a -> Statement '[] a
setStatementIndents desired = transform (_Indent .~ desired)

setModuleIndents :: [Whitespace] -> Module '[] a -> Module '[] a
setModuleIndents = over _Statements . setStatementIndents

parseNamedModule :: Named Text -> Validation (NonEmpty (ParseError SrcInfo)) (Named (Module '[] SrcInfo))
parseNamedModule (Named n a) = Named n <$> HPY.parseModule n a

parseNamedModules :: [Named Text] -> Validation (NonEmpty (ParseError SrcInfo)) [Named (Module '[] SrcInfo)]
parseNamedModules = traverse parseNamedModule

---- main
main :: IO ()
main = do
  opts <- parseOpts
  let desiredIndent = desiredIndentation opts
  filePaths <- getDirTrees (optFiles opts)
  files <- readNamedFiles filePaths
  case parseNamedModules files of
    Failure e -> do
      putStrLn "The following errors occurred:"
      traverse_ print e
    Success mods -> do
      let mods' = fmap (fmap (setModuleIndents desiredIndent)) mods
      traverse_ (writeNamedFile . fmap showModule) mods'
