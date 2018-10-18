{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reindent.Transformation.Indentation (DesiredIndentation (..), reindent) where

import Control.Lens ((.~), toListOf, over, to)
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Char (isSpace)
import Data.List (replicate)
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
reindent di = allStatements (reindentMultiLineStrings . reindentStatement (expandDI di))
  where
    reindentStatement :: [Whitespace] -> Statement '[] a -> Statement '[] a
    reindentStatement desired = (_Indent .~ desired)

    reindentMultiLineStrings :: Applicative f => Statement '[] a -> Statement '[] a
    reindentMultiLineStrings smnt =
      case smnt of
        x@(CompoundStatement _)             -> x
        x@(SimpleStatement _ (MkSimpleStatement _ (_:_) _ _ _)) -> x
        SimpleStatement idnt (MkSimpleStatement a []    c d e)  ->
          SimpleStatement idnt (MkSimpleStatement (reindentString idnt a) [] c d e)
      where
        reindentString idnt (Expr a (String a2 nel)) = Expr a (String a2 (fmap (modifyLiterals idnt) nel))
        reindentString _    x                        = x

        modifyLiterals idnt =
          over stringLiteralValue (reindentLiteral idnt)

    alterchunks :: [PyChar] -> AlterChunks [PyChar] [Newline]
    alterchunks = foldMap f
      where
        f :: PyChar -> AlterChunks [PyChar] [Newline]
        f pc =
          case pc of
            Char_lit '\r'  -> inR [CR]
            Char_lit '\n'  -> inR [LF]
            c@(Char_lit _) -> inL [c]
            x              -> inL [x]

    reindentLiteral :: Indents a -> [PyChar] -> [PyChar]
    reindentLiteral idnts input =
      let chunks = alterchunks input
      in  go chunks
      where
        go :: AlterChunks [PyChar] [Newline] -> [PyChar]
        go (AC Empty) = []
        go (AC (Left x  :<| tl))    = x <> go (AC tl)
        go (AC (Right x :<| Empty)) = nlpc x
        go (AC (Right x :<| Left y  :<| tl)) = reindentLine x y <> go (AC tl)
        go (AC (Right x :<| Right y :<| tl)) = nlpc x <> nlpc y <> go (AC tl)

        nlpc = foldMap displayNewline
        wspc = fmap Char_lit . unpack . showRenderOutput . renderWhitespace

        reindentLine :: [Newline] -> [PyChar] -> [PyChar]
        reindentLine nl pc =
          nlpc nl <> idnts' <> stripPrefix pc
            where
              idnts' = toListOf (
                  indentsValue.traverse
                  .indentWhitespaces.traverse
                  .to wspc.traverse
                ) idnts

    stripPrefix :: [PyChar] -> [PyChar]
    stripPrefix str =
      let isSpacePC :: PyChar -> Bool
          isSpacePC (Char_lit c) = isSpace c
          isSpacePC _ = False
      in  dropWhile isSpacePC str

    displayNewline :: Newline -> [PyChar]
    displayNewline nl = Char_lit <$> case nl of
      CR   -> "\r"
      LF   -> "\n"
      CRLF -> "\r\n"

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
      (Left  x, Left  y) -> AC sinit <> AC (Seq.singleton (Left  (x <> y))) <> AC stail
      (Right x, Right y) -> AC sinit <> AC (Seq.singleton (Right (x <> y))) <> AC stail
      (Left  _, Right _) -> AC $ sinit <> (Seq.fromList [slast, shead]) <> stail
      (Right _, Left  _) -> AC $ sinit <> (Seq.fromList [slast, shead]) <> stail

instance (Semigroup a, Semigroup b) => Monoid (AlterChunks a b) where
  mempty = AC mempty
  mappend = (<>)

instance Bifunctor AlterChunks where
  bimap = bimapDefault

instance Bifoldable AlterChunks where
  bifoldMap = bifoldMapDefault

instance Bitraversable AlterChunks where
  bitraverse f g (AC s) = AC <$> traverse (bitraverse f g) s
