{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reindent.Transformation.Indentation (DesiredIndentation (..), reindentLexically) where

import Control.Lens ((.~))
import Data.List (replicate)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Validation (Validation (..))

import Language.Python.Optics (_Indent)
import Language.Python.Internal.Render (showTokens)
import Language.Python.Internal.Lexer (initialSrcInfo, insertTabs, tokenize)
import Language.Python.Syntax.Whitespace (Whitespace (Space, Tab))

data DesiredIndentation =
  DITab | DISpaces Int
  deriving (Eq, Ord, Show)

expandDI :: DesiredIndentation -> [Whitespace]
expandDI di = case di of
  DITab      -> [Tab]
  DISpaces i -> replicate i Space

reindentLexically
  :: DesiredIndentation
  -> FilePath
  -> Text
  -> Validation (NonEmpty String) Text
reindentLexically di fp m =
  let
    ws = expandDI di
    si = initialSrcInfo fp
    tokens = insertTabs si <$> tokenize fp m
    e = fmap (traverse.traverse._Indent .~ ws) tokens
  in
    case e of
      Left es         -> Failure (pure (show es))
      Right (Left es) -> Failure (pure (show es))
      Right (Right x) -> Success (showTokens x)
