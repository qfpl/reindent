module Main where

import Control.Applicative ((<$>), (<**>), many)
import qualified Options.Applicative as O
import Language.Python.Parse

data IndentOptions =
  IndentOptions [FilePath]
  deriving (Eq, Ord, Show)

indentOptions :: O.Parser IndentOptions
indentOptions = IndentOptions <$> many pyfile <**> O.helper

pyfile :: O.Parser FilePath
pyfile =
  O.strArgument $
    mconcat [O.metavar "python file", O.help "Python source file to reindent"]

parseOpts :: IO IndentOptions
parseOpts = O.execParser . O.info indentOptions $ mconcat
  [O.fullDesc, O.header "reindent - fix inconsistent python indentation"]

main :: IO ()
main = do
  opts <- parseOpts
  print opts
