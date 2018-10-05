module Reindent.FileIO where

import Data.List (isPrefixOf, isSuffixOf)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import System.Directory

-- some data along with its filepath
data Named a =
  Named {
    name  :: FilePath
  , value :: a
  }

instance Functor Named where
  fmap f (Named n a) = Named n (f a)

instance Foldable Named where
  foldMap f (Named _ a) = f a

instance Traversable Named where
  traverse f (Named n a) = Named n <$> f a

writeNamedFile :: Named Text -> IO ()
writeNamedFile (Named fp text) = Text.writeFile fp text

readNamedFile :: FilePath -> IO (Named Text)
readNamedFile fp = traverse Text.readFile (Named fp fp)

readNamedFiles :: [FilePath] -> IO [Named Text]
readNamedFiles = traverse readNamedFile

isPythonFile :: FilePath -> Bool
isPythonFile = isSuffixOf ".py"

isHidden :: FilePath -> Bool
isHidden = isPrefixOf "."

getDirTree :: FilePath -> IO [FilePath]
getDirTree fp = do
  isFile <- doesFileExist fp
  if isFile then
    if isPythonFile fp then pure [fp] else pure []
  else do
    isDir <- doesDirectoryExist fp
    if not isDir then pure []
    else do
      contents <- filter (not . isHidden) <$> listDirectory fp
      let fps = (\x -> fp ++ "/" ++ x) <$> contents
      concat <$> traverse getDirTree fps

getDirTrees :: [FilePath] -> IO [FilePath]
getDirTrees = fmap concat . traverse getDirTree

