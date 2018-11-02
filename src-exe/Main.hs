{-# LANGUAGE DataKinds #-}

module Main where

import Control.Lens.Indexed (TraversableWithIndex (..))
import Data.Bifoldable (bitraverse_)
import Data.Foldable (traverse_)
import System.Exit (exitFailure)

import Reindent.FileIO (getDirTrees, readNamedFiles, writeNamedFile)
import Reindent.Options (desiredIndentation, optFiles, parseOpts)
import Reindent.Transformation (reindentLexically)

main :: IO ()
main = do
  opts <- parseOpts
  let desiredIndent = desiredIndentation opts
  let reindent = reindentLexically desiredIndent
  filePaths <- getDirTrees (optFiles opts)
  files <- readNamedFiles filePaths
  let x = traverse (itraverse reindent) files
  bitraverse_ printFailures handleSuccesses x
    where
      printFailures xs = do
        putStrLn "Something went wrong"
        traverse_ print xs
        exitFailure
      handleSuccesses = traverse_ writeNamedFile
