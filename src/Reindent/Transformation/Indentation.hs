{-# LANGUAGE DataKinds #-}

module Reindent.Transformation.Indentation (DesiredIndentation (..), reindent) where

import Control.Lens ((.~))
import Data.List (replicate)

import Language.Python.Internal.Optics
import Language.Python.Internal.Syntax

import Reindent.Transformation.Type

data DesiredIndentation =
  DITab | DISpaces Int
  deriving (Eq, Ord, Show)

setStatementIndents :: [Whitespace] -> Statement '[] a -> Statement '[] a
setStatementIndents desired = (_Indent .~ desired)

reindent :: Applicative f => DesiredIndentation -> Pypeline f '[] a
reindent = allStatements . setStatementIndents . expandDI

expandDI :: DesiredIndentation -> [Whitespace]
expandDI di = case di of
  DITab -> [Tab]
  DISpaces i -> replicate i Space
