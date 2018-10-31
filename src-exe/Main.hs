{-# LANGUAGE DataKinds #-}

module Main where

import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Validation (Validation (Success, Failure))
import System.Exit (exitFailure)

import Language.Python.Parse as HPY
import Language.Python.Render (showModule)
import Language.Python.Syntax.Module (Module)

import Reindent.FileIO (Named (Named), getDirTrees, readNamedFiles, writeNamedFile)
import Reindent.Options (desiredIndentation, optFiles, parseOpts)
import Reindent.Transformation (reindent)

parseNamedModule :: Named Text -> Validation (NonEmpty (ParseError SrcInfo)) (Named (Module '[] SrcInfo))
parseNamedModule (Named n a) = Named n <$> HPY.parseModule n a

parseNamedModules :: [Named Text] -> Validation (NonEmpty (ParseError SrcInfo)) [Named (Module '[] SrcInfo)]
parseNamedModules = traverse parseNamedModule

---- main
main :: IO ()
main = do
  opts <- parseOpts
  let desiredIndent = desiredIndentation opts
  let refactor = reindent desiredIndent
  filePaths <- getDirTrees (optFiles opts)
  files <- readNamedFiles filePaths
  case parseNamedModules files of
    Failure e -> do
      putStrLn "The following errors occurred:"
      traverse_ print e
      exitFailure
    Success mods ->
      traverse_ (writeNamedFile . fmap (showModule . refactor)) mods
