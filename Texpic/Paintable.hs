{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, StandaloneDeriving, KindSignatures, TypeFamilies, LambdaCase #-}
module Texpic.Paintable where

import Data.Bifunctor
import Texpic.Color
import Data.Functor.Foldable.TH
import Data.Functor.Foldable


data Paintable a =
  Empty 
  | Curve [a]
  | Line a a
  | Arc a a a
  | Tex a String
  | Composed (Paintable a) (Paintable a)
  | MetaThick (Paintable a)
  | MetaColor Color (Paintable a)
  deriving (Eq,Ord,Show,Functor)

makeBaseFunctor ''Paintable

{-
instance Bifunctor PaintableF where
  first f = \case
    CurveF a's -> CurveF (fmap f a's)
    LineF a1 a2 -> LineF (f a1) (f a2)
    ArcF a1 a2 a3 -> ArcF (f a1) (f a2) (f a3)
    TexF a s -> TexF (f a) s
    ComposedF x1 x2 -> ComposedF x1 x2
    MetaThickF x -> MetaThickF x
    MetaColorF col x -> MetaColorF col x
  second = fmap
-}

instance Monoid (Paintable a) where
  mempty = Empty
  mappend = Composed

discardColor :: Paintable a -> Paintable a
discardColor = cata $ \case
  MetaColorF _ x -> x
  fx -> embed fx

discardThick :: Paintable a -> Paintable a
discardThick = cata $ \case
  MetaThickF x -> x
  fx -> embed fx


