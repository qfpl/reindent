{-# LANGUAGE DataKinds #-}

module Main where

import Control.Applicative ((<$>), (<**>), (<|>), liftA2, many)
import Control.Lens
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Validation (Validation (Success, Failure))
import qualified Options.Applicative as O

import Language.Python.Parse as HPY
import Language.Python.Internal.Optics
import Language.Python.Internal.Render
import Language.Python.Internal.Syntax

import Reindent.FileIO

---- Options parsing
data AppOptions =
  AppOptions
    { desiredIndentation :: [Whitespace]
    , optFiles :: [FilePath]
    }
  deriving (Eq, Show)

appOptions :: O.Parser AppOptions
appOptions = AppOptions <$> indentationOpts <*> many pyfile <**> O.helper

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

parseModuleN :: Named Text -> Validation (NonEmpty (ParseError SrcInfo)) (Named (Module '[] SrcInfo))
parseModuleN (Named n a) = Named n <$> HPY.parseModule n a

---- main
main :: IO ()
main = do
  opts <- parseOpts
  let desiredIndent = desiredIndentation opts
  filePaths <- getDirTrees (optFiles opts)
  files <- readNamedFiles filePaths
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
