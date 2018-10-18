{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Reindent.Transformation.Type where

import Prelude hiding ((.),id)
import Control.Category (Category ((.), id))
import Control.Lens (Wrapped, Unwrapped, makeWrapped, over, transform, view, _Unwrapped', _Wrapped')
import Control.Monad ((<=<))
import Data.Functor.Identity (Identity (runIdentity))
import Data.Semigroup (Semigroup ((<>)))

import Language.Python.Internal.Syntax (Module, Statement, _Statements)

---- Types
newtype Pyfactor f j k a b = Pyfactor { runPyfactor :: Module j a -> f (Module k b) }
type Pyfactor' = Pyfactor Identity

newtype Pypeline f j a = Pypeline { getPypeline :: Pyfactor f j j a a }
type Pypeline' = Pypeline Identity

instance Monad f => Semigroup (Pypeline f j a) where
  Pypeline (Pyfactor m) <> Pypeline (Pyfactor n) = Pypeline (Pyfactor (m <=< n))

instance Monad f => Monoid (Pypeline f j a) where
  mappend = (<>)
  mempty = Pypeline (Pyfactor pure)

---- Creating Pypelines
pypeline :: (Module j a -> f (Module j a)) -> Pypeline f j a
pypeline = Pypeline . Pyfactor

pypeline' :: Applicative f => (Module j a -> Module j a) -> Pypeline f j a
pypeline' = pypeline . fmap pure

allStatements :: Applicative f => (Statement '[] a -> Statement '[] a) -> Pypeline f '[] a
allStatements = pypeline' . over _Statements . transform

---- Running Pypelines
runPypeline :: Pypeline f j a -> Module j a -> f (Module j a)
runPypeline = runPyfactor . getPypeline

runPypeline' :: Pypeline' j a -> Module j a -> Module j a
runPypeline' = fmap runIdentity . runPypeline

---- Newtypes
newtype Pyannotate f j a b = Pyannotate (Pyfactor f j j a b)
type Pyannotate' = Pyannotate Identity

newtype Pyvalidate f a j k = Pyvalidate (Pyfactor f j k a a)
type Pyvalidate' = Pyvalidate Identity

instance Monad f => Category (Pyannotate f j) where
  id = Pyannotate (Pyfactor pure)
  (.) = composeW

instance Monad f => Category (Pyvalidate f a) where
  id = Pyvalidate (Pyfactor pure)
  (.) = composeW

---- Implementation details
compose :: Monad f => Pyfactor f j1 k a1 b -> Pyfactor f j2 j1 a2 a1 -> Pyfactor f j2 k a2 b
compose (Pyfactor m) (Pyfactor n) = Pyfactor (m <=< n)

composeW ::
  (Wrapped t, Wrapped s1, Wrapped s2
  , Monad f
  , Unwrapped s1 ~ Pyfactor f j1 k a1 b, Unwrapped s2 ~ Pyfactor f j2 j1 a2 a1, Unwrapped t ~ Pyfactor f j2 k a2 b
  ) => s1 -> s2 -> t
composeW p q = view _Unwrapped' $ view _Wrapped' p `compose` view _Wrapped' q

makeWrapped ''Pyfactor
makeWrapped ''Pyannotate
makeWrapped ''Pyvalidate
