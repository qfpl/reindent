{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reindent.Transformation.Indentation (DesiredIndentation (..), reindent) where

import Control.Applicative (liftA2)
import Control.Lens ((.~), over)
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Char (isSpace)
import Data.List (replicate)
--import Data.List.Split (splitOn)
import Data.Sequence (Seq (Empty, (:<|), (:|>)))
import qualified Data.Sequence as Seq
import Data.Text (unpack)

import Language.Python.Internal.Optics
import Language.Python.Internal.Render (renderWhitespace, showRenderOutput)
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
        x@(SimpleStatement _ (MkSimpleStatement _ (_:_) _ _ _)) -> x
        SimpleStatement idnt (MkSimpleStatement a []    c d e)  ->
          SimpleStatement idnt (MkSimpleStatement (reindentString desired a) [] c d e)
      where
        reindentString dsrd (Expr a (String a2 nel)) = Expr a (String a2 (fmap (modifyLiterals dsrd) nel))
        reindentString _    x                           = x

        modifyLiterals dsrd =
          over stringLiteralValue (reindentLiteral dsrd)

    alterchunks :: [PyChar] -> AlterChunks [PyChar] [Newline]
    alterchunks = foldMap f
      where
        f :: PyChar -> AlterChunks [PyChar] [Newline]
        f pc =
          bimap pure pure $ case pc of
            Char_lit '\r'  -> inR CR
            Char_lit '\n'  -> inR LF
            c@(Char_lit _) -> inL c
            x              -> inL x

    reindentLiteral :: [Whitespace] -> [PyChar] -> [PyChar]
    reindentLiteral desired input =
      let chunks = alterchunks input
      in  go chunks
      where

        go :: AlterChunks [PyChar] [Newline] -> [PyChar]
        go (AC Empty) = []
        go (AC (Left x  :<| tl))    = x <> go (AC tl)
        go (AC (Right x :<| Empty)) = nlpc x
        go (AC (Right x :<| Left y  :<| tl)) = combine x y <> go (AC tl)
        go (AC (Right x :<| Right y :<| tl)) = nlpc x <> nlpc y <> go (AC tl)

        nlpc = foldMap displayNewline

        combine :: [Newline] -> [PyChar] -> [PyChar]
        combine nl pc = nlpc nl <> reindentLine desired pc

    reindentLine :: [Whitespace] -> [PyChar] -> [PyChar]
    reindentLine desired str =
      let noPrefix = dropWhile isSpacePC str
          isSpacePC :: PyChar -> Bool
          isSpacePC (Char_lit c) = isSpace c
          isSpacePC _ = False
          newPrefix = Char_lit <$> unpack (showRenderOutput (foldMap renderWhitespace desired))
      in  if   null noPrefix
          then noPrefix
          else newPrefix <> noPrefix

    displayNewline :: Newline -> [PyChar]
    displayNewline nl = Char_lit <$> case nl of
      CR   -> "\r"
      LF   -> "\n"
      CRLF -> "\r\n"

--type Whichever = Tannen [] Either

--data Alternate a b
--  = NilA
--  | ConsA a (Alternate b a)
--  deriving (Eq, Ord, Show)

--instance Bifunctor Alternate where
--  bimap = bimapDefault

--instance Bifoldable Alternate where
--  bifoldMap = bifoldMapDefault

--instance Bitraversable Alternate where
--  bitraverse f g (ConsA a tl) = ConsA <$> f a <*> bitraverse g f tl
--  bitraverse _ _ NilA         = pure NilA

--instance Functor (Alternate a) where
--  fmap = fmapDefault

--instance Foldable (Alternate a) where
--  foldMap = foldMapDefault

--instance Traversable (Alternate a) where
--  traverse = bitraverse pure



-- JUST WRITE A MONOID FOR IT

data AlterChunks a b =
  AC (Seq (Either a b))

inL :: a -> AlterChunks a b
inL = AC . Seq.singleton . Left

inR :: b -> AlterChunks a b
inR = AC . Seq.singleton . Right

instance (Semigroup a, Semigroup b) => Semigroup (AlterChunks a b) where
  x     <> AC Empty = x
  AC Empty <> y     = y
  AC (sinit :|> slast) <> AC (shead :<| stail) =
    case (slast, shead) of
      (Left  x, Left y)  -> AC sinit <> AC (Seq.singleton (Left  (x <> y))) <> AC stail
      (Right x, Right y) -> AC sinit <> AC (Seq.singleton (Right (x <> y))) <> AC stail
      (Left  _, Right _) -> AC $ sinit <> (Seq.fromList [slast, shead]) <> stail
      (Right _, Left _)  -> AC $ sinit <> (Seq.fromList [slast, shead]) <> stail

instance (Semigroup a, Semigroup b) => Monoid (AlterChunks a b) where
  mempty = AC mempty
  mappend = (<>)

instance Bifunctor AlterChunks where
  bimap = bimapDefault

instance Bifoldable AlterChunks where
  bifoldMap = bifoldMapDefault

instance Bitraversable AlterChunks where
  bitraverse f g (AC s) = AC <$> traverse (bitraverse f g) s
