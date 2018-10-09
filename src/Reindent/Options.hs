module Reindent.Options where

import Control.Applicative ((<$>), (<**>), (<|>), liftA2, many)
import qualified Options.Applicative as O

import Reindent.Indentation (DesiredIndentation (DITab, DISpaces))

data AppOptions =
  AppOptions
    { desiredIndentation :: DesiredIndentation
    , optFiles :: [FilePath]
    }
  deriving (Eq, Show)

appOptions :: O.Parser AppOptions
appOptions = AppOptions <$> indentationOpts <*> many pyfile <**> O.helper

indentationOpts :: O.Parser DesiredIndentation
indentationOpts =
  liftA2 f tab spaces
    where
      f b i = if b then DITab else DISpaces i
      tab = O.switch $ mconcat
        [ O.long "tabs"
        , O.help "Replace indentation with tabs"]
      spaces = O.option O.auto (mconcat
        [ O.long "spaces"
        , O.metavar "num"
        , O.help "Replace indentation with spaces"
        ]) <|> pure defaultSpaces
      defaultSpaces = 4

pyfile :: O.Parser FilePath
pyfile =
  O.strArgument $
    mconcat [O.metavar "python-file", O.help "Python source file to reindent"]

parseOpts :: IO AppOptions
parseOpts = O.execParser . O.info appOptions $ mconcat
  [O.fullDesc, O.header "reindent - fix inconsistent python indentation"]
