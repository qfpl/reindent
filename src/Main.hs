module Main where

import Control.Applicative ((<$>), (<**>), liftA2, many)
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

data IndentOptions =
  IndentOptions
    { optFiles :: NonEmpty FilePath
    }
  deriving (Eq, Ord, Show)

indentOptions :: O.Parser IndentOptions
indentOptions = IndentOptions <$> many1 pyfile <**> O.helper
  where
    many1 p = liftA2 (:|) p (many p)

pyfile :: O.Parser FilePath
pyfile =
  O.strArgument $
    mconcat [O.metavar "python-file", O.help "Python source file to reindent"]

parseOpts :: IO IndentOptions
parseOpts = O.execParser . O.info indentOptions $ mconcat
  [O.fullDesc, O.header "reindent - fix inconsistent python indentation"]

desiredInd :: Indent
desiredInd = review indentWhitespaces []

main :: IO ()
main = do
  opts <- parseOpts
  files <- traverse (\x -> traverse Text.readFile (x,x)) (optFiles opts)
  let parsedModules = traverse (uncurry HPY.parseModule) files
  case parsedModules of
    Failure e -> do
      putStrLn "The following errors occurred:"
      traverse_ print e
    Success mods -> do
      let mods' = over (_Statements._Indents.indentsValue.traverse) (const desiredInd) <$> mods
      traverse_ (Text.putStrLn . showModule) mods'
