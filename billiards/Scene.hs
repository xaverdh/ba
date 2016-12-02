module Scene ( mkScene )where

import Numeric.LinearAlgebra
import Data.List (unfoldr,last)
import Control.Monad.Reader

import Paintable
import OuterBilliard
import OuterBilliardCurve
import ConfigParser

paintCurve :: Reader Config (Paintable C)
paintCurve = do
  col <- asks configCurveColor
  obc <- asks configCurve
  let g = gamma obc
      l = lenParam obc
      points = map (*(l / 100)) [0..100]
  pure $ Curve [g t | t <- points ] col


tracePath :: Reader Config [Paintable C]
tracePath = do
  p <- asks configStart
  lcol <- asks configLineColor
  let
    mkLine x y = Line x y lcol
    macc f x = do
      y <- f x
      ys <- macc f y
      return (y:ys)
  path <- macc reflect p
  pure $ zipWith mkLine path (tail path)


mkScene :: Reader Config [Paintable C]
mkScene = do
  path <- tracePath
  curve <- paintCurve
  pure $ curve:path



