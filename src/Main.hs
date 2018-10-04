{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Main where

import Control.Applicative ((<$>), (<**>), (<|>), liftA2, many)
import Control.Lens
import Data.Foldable (traverse_)
import Data.List (isPrefixOf, isSuffixOf)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Data.Validation (Validation (Success, Failure))
import qualified Options.Applicative as O
import System.Directory

import Language.Python.Parse as HPY
import Language.Python.Internal.Optics
import Language.Python.Internal.Render
import Language.Python.Internal.Syntax


---- Options parsing
data AppOptions =
  AppOptions
    { desiredIndentation :: [Whitespace]
    , optFiles :: [FilePath]
    }
  deriving (Eq, Ord, Show)

appOptions :: O.Parser AppOptions
appOptions = AppOptions <$> indentationOpts <*> many pyfile <**> O.helper
  --where
    --many1 p = liftA2 (:|) p (many p)

indentationOpts :: O.Parser [Whitespace]
indentationOpts =
  liftA2 f tab spaces
    where
      f b i = if b then [Tab] else replicate i Space
      tab = O.switch $ mconcat
        [ O.long "tabs"
        , O.help "Replace indentation with tabs"]
      spaces = O.option O.auto (mconcat
        [ O.long "spaces"
        , O.metavar "num"
        , O.help "Replace indentation with spaces"
        ]) <|> pure 4

pyfile :: O.Parser FilePath
pyfile =
  O.strArgument $
    mconcat [O.metavar "python-file", O.help "Python source file to reindent"]

parseOpts :: IO AppOptions
parseOpts = O.execParser . O.info appOptions $ mconcat
  [O.fullDesc, O.header "reindent - fix inconsistent python indentation"]


---- Indentation stuff
setStatementIndents :: [Whitespace] -> Statement '[] a -> Statement '[] a
setStatementIndents desired = transform (_Indent .~ desired)

setModuleIndents :: [Whitespace] -> Module '[] a -> Module '[] a
setModuleIndents = over _Statements . setStatementIndents


---- File IO

data Named a =
  Named {
    name  :: FilePath
  , value :: a
  } deriving (Functor, Foldable, Traversable)

writeFileN :: Named Text -> IO ()
writeFileN (Named fp text) = Text.writeFile fp text

parseModuleN :: Named Text -> Validation (NonEmpty (ParseError SrcInfo)) (Named (Module '[] SrcInfo))
parseModuleN (Named n a) = Named n <$> HPY.parseModule n a

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

---- main
main :: IO ()
main = do
  opts <- parseOpts
  let desiredIndent = desiredIndentation opts
  filePaths <- getDirTrees (optFiles opts)
  files <- traverse (\fp -> traverse Text.readFile (Named fp fp)) filePaths
  let
    parsedModules :: Validation (NonEmpty (ParseError SrcInfo)) [Named (Module '[] SrcInfo)]
    parsedModules = traverse parseModuleN files
  case parsedModules of
    Failure e -> do
      putStrLn "The following errors occurred:"
      traverse_ print e
    Success mods -> do
      let mods' = fmap (fmap (setModuleIndents desiredIndent)) mods
      traverse_ (writeFileN . fmap showModule) mods'
