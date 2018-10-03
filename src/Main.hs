module Main where

import Control.Applicative ((<$>), (<**>), (<|>), liftA2, many)
import Control.Lens
import Data.Foldable (traverse_, for_)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Options.Applicative as O
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Validation (Validation (Success, Failure))

import Language.Python.Parse as HPY
import Language.Python.Internal.Optics
import Language.Python.Internal.Render
import Language.Python.Internal.Syntax
import Language.Python.Internal.Syntax.Whitespace


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
      spaces = O.option (O.auto) (mconcat
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
setStatementIndents desired = transform (_Indent .~ desired)

setModuleIndents = over _Statements . setStatementIndents


---- main
main :: IO ()
main = do
  opts <- parseOpts
  let desiredIndent = desiredIndentation opts
  files <- traverse (\x -> traverse Text.readFile (x,x)) (optFiles opts)
  let parsedModules = traverse (uncurry HPY.parseModule) files
  case parsedModules of
    Failure e -> do
      putStrLn "The following errors occurred:"
      traverse_ print e
    Success mods -> do
      let mods' = fmap (setModuleIndents desiredIndent) mods
      traverse_ (Text.putStrLn . showModule) mods'
