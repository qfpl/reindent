{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reindent.Transformation.Indentation (DesiredIndentation (..), reindent) where

import Control.Applicative (liftA2)
import Control.Lens ((.~))
import Data.Bifunctor
import Data.Bifunctor.Tannen
import Data.Bifoldable
import Data.Bitraversable
import Data.List (replicate)
import Data.List.Split (splitOn)
import Data.Traversable (Traversable (..), fmapDefault, foldMapDefault)

import Language.Python.Internal.Optics
import Language.Python.Internal.Syntax.Statement
import Language.Python.Internal.Syntax

import Reindent.Transformation.Type

data DesiredIndentation =
  DITab | DISpaces Int
  deriving (Eq, Ord, Show)

expandDI :: DesiredIndentation -> [Whitespace]
expandDI di = case di of
  DITab      -> [Tab]
  DISpaces i -> replicate i Space

reindent :: forall f a. Applicative f => DesiredIndentation -> Pypeline f '[] a
reindent = allStatements . (liftA2 (.) reindentStatements reindentMultiLineStrings) . expandDI
  where
    reindentStatements :: [Whitespace] -> Statement '[] a -> Statement '[] a
    reindentStatements desired = (_Indent .~ desired)

    reindentMultiLineStrings :: Applicative f => [Whitespace] -> Statement '[] a -> Statement '[] a
    reindentMultiLineStrings desired smnt =
      case smnt of
        x@(CompoundStatement _)             -> x
        x@(SmallStatements _ _ (_:_) _ _ _) -> x
        SmallStatements idnt a []    c d e  ->
          SmallStatements idnt (reindentString desired a) [] c d e
      where
        reindentString desired (Expr a (String a2 nel)) = Expr a (String a2 (fmap (g desired) nel))
        reindentString _    x                        = x

        g desired stlit = case stlit of
          (StringLiteral _ _ _ _ _ _) -> undefined
          (RawBytesLiteral _ _ _ _ _ _) -> undefined
          (BytesLiteral _ _ _ _ _ _) -> undefined
          (RawStringLiteral _ _ _ _ _ _) -> undefined

    -- joinWithNewlines . splitOnNewlines = id
    splitOnNewlines :: String -> Alternate String Newline
    splitOnNewlines = undefined
    joinWithNewlines :: Alternate String Newline -> String
    joinWithNewlines = undefined
    alterLines :: Monoid m => (String -> m) -> (Newline -> m) -> String -> m
    alterLines f g = bifoldMap f g . splitOnNewlines

    reindentLiteral :: [Whitespace] -> String -> String
    reindentLiteral desired = alterLines (reindentLine desired) displayNewline

    reindentLine :: [Whitespace] -> String -> String
    reindentLine desired str =
      let noPrefix = dropWhile isSpace
      in  if   null noPrefix
          then noPrefix
          else _

    displayNewline :: String -> String
    displayNewline nl = case nl of
      CR   -> "\r"
      LF   -> "\n"
      CRLF -> "\r\n"

type Alternate = Tannen [] Either

-- Maybe replace with a list of eithers
{-
data Alternate a b
  = NilA
  | ConsA a (Alternate b a)
  deriving (Eq, Ord, Show)

instance Bifunctor Alternate where
  bimap = bimapDefault

instance Bifoldable Alternate where
  bifoldMap = bifoldMapDefault

instance Bitraversable Alternate where
  bitraverse f g (ConsA a tl) = ConsA <$> f a <*> bitraverse g f tl
  bitraverse _ _ NilA         = pure NilA

instance Functor (Alternate a) where
  fmap = fmapDefault

instance Foldable (Alternate a) where
  foldMap = foldMapDefault

instance Traversable (Alternate a) where
  traverse = bitraverse pure
-}
