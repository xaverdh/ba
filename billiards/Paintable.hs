{-# LANGUAGE GADTs #-}
module Paintable ( Paintable(..), Color(..) ) where

import Color

data Paintable a where
  Curve :: [a] -> Color -> Paintable a
  Line :: a -> a -> Color -> Paintable a
  Arc ::  a -> a -> a -> Color -> Paintable a
  Circle :: a -> Double -> Color -> Paintable a
  deriving (Show)

