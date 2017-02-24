{-# LANGUAGE GADTs, TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, StandaloneDeriving, KindSignatures, TypeFamilies #-}
module Texpic.Paintable where

import Data.Bifunctor
import Texpic.Color
import Data.Functor.Foldable.TH
import Data.Functor.Foldable

{-
data PaintableF a x =
  EmptyF
  | CurveF [a]
  | LineF a a
  | ArcF a a a
  | TexF a String
  | ComposedF x x
  | MetaThickF x
  | MetaColorF Color x
  deriving (Eq,Ord,Show)

deriving instance Functor (PaintableF a)


instance Bifunctor PaintableF where
  first f p = case p of
    CurveF a's -> CurveF (fmap f a's)
    LineF a1 a2 -> LineF (f a1) (f a2)
    ArcF a1 a2 a3 -> ArcF (f a1) (f a2) (f a3)
    TexF a s -> TexF (f a) s
    ComposedF x1 x2 -> ComposedF x1 x2
    MetaThickF x -> MetaThickF x
    MetaColorF col x -> MetaColorF col x
  second g = fmap g

cata :: (PaintableF a x -> x) -> Paintable a -> x
cata phi = phi . fmap (cata phi) . outF

outF :: Paintable a -> PaintableF a (Paintable a)
outF p = case p of
    Empty -> EmptyF
    Curve a's -> CurveF a's
    Line a1 a2 -> LineF a1 a2
    Arc a1 a2 a3 -> ArcF a1 a2 a3
    Tex a s -> TexF a s
    Composed p1 p2 -> ComposedF p1 p2
    MetaThick p' -> MetaThickF p'
    MetaColor col p' -> MetaColorF col p'

inF :: PaintableF a (Paintable a) -> Paintable a
inF fx = case fx of
    EmptyF -> Empty
    CurveF a's -> Curve a's
    LineF a1 a2 -> Line a1 a2
    ArcF a1 a2 a3 -> Arc a1 a2 a3
    TexF a s -> Tex a s
    ComposedF x1 x2 -> Composed x1 x2
    MetaThickF x -> MetaThick x
    MetaColorF col x -> MetaColor col x
-}

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

instance Monoid (Paintable a) where
  mempty = Empty
  mappend = Composed


discardColor :: Paintable a -> Paintable a
discardColor = cata $
  \fx -> case fx of
    MetaColorF _ x -> x
    _ -> embed fx

discardThick :: Paintable a -> Paintable a
discardThick = cata $
  \fx -> case fx of
    MetaThickF x -> x
    _ -> embed fx
  

