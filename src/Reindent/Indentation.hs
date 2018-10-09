{-# LANGUAGE DataKinds #-}

module Reindent.Indentation where

import Control.Lens ((.~))

import Language.Python.Internal.Optics
import Language.Python.Internal.Syntax

import Reindent.Transformations

setStatementIndents :: [Whitespace] -> Statement '[] a -> Statement '[] a
setStatementIndents desired = (_Indent .~ desired)

setModuleIndents :: Applicative f => [Whitespace] -> Pypeline f '[] a
setModuleIndents = allStatements . setStatementIndents
