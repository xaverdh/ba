{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
module OuterBilliard where

import Numeric.GSL.Root
import Numeric.GSL.Minimization
import Numeric.LinearAlgebra hiding (toList,fromList)
import Data.Fixed (mod')
import Data.Bool (bool)
import Control.Monad.Reader
import GHC.Exts (IsList(..))
import OuterBilliardCurve
import ConfigParser

import Debug.Trace (trace)

instance IsList (Complex a) where
  type Item (Complex a) = a
  toList (x :+ y) = [x,y]
  fromList [x,y] = (x :+ y)


{-# INLINE inner #-}
inner :: C -> C -> R
inner (x1 :+ x2) (y1 :+ y2) = x1*y1 + x2*y2

{-# INLINE (.>) #-}
(.>) :: R -> C -> C
l .> (x :+ y) = (l*x :+ l*y)

{-# INLINE (<.) #-}
(<.) :: C -> R -> C
(x :+ y) <. l = (l*x :+ l*y)

{-# INLINE norm #-}    
norm :: C -> R
norm = magnitude
-- norm (x :+ y) = sqrt (x^2+y^2)

{-# INLINE normSquared #-}
normSquared :: C -> R
normSquared (x :+ y) = x^2 + y^2

{-# INLINE normalise #-}    
normalise :: C -> C
normalise (x :+ y) = 
  let n = sqrt (x^2+y^2)-- (x**2+y**2)
  in (x/n :+ y/n)


{-
  We take abs l in tangent and tangent'
  since we really want to go only in one
  direction.
  The resulting function strictly speaking
  ins't differentiable at 0, but numeric
  algorithms are rarely bothered by that.
-}
tangent :: OuterBilliardCurve -> C -> C
tangent (OBC { gamma = g, gamma' = g'}) (t :+ l) = 
  g t + abs l .> g' t

tangent' :: OuterBilliardCurve -> C -> [C]
tangent' (OBC { gamma' = g', gamma'' = g'' }) (t :+ l) =
  [
    g' t + abs l .> g'' t,
    g' t
  ]


invTangentSpreadInv :: C -> Reader Config C
invTangentSpreadInv p = do
  obc <- asks configCurve
  prec <- asks configInvFirstShotPrecision
  iters <- asks configInvFirstShotIterations
  noj <- asks configInvFirstShotNoJ
  simple <- asks configInvFirstShotSimple
  let
    ts = map (*(lenParam obc / n)) [1..n]
    cs =  [ t :+ magnitude (gamma obc t - p) | t <- ts ]
    
    residual c = (magnitude (tangent obc c-p),c)

    inv
      | simple = pure id
      | noj = invNoJ
      | otherwise = invJ
    
    invNoJ = do
      method <- asks configInvFirstShotMethodNoJ
      pure $ fromList . fst . root method prec iters f . toList
    
    invJ = do
      method <- asks configInvFirstShotMethod
      pure $ fromList . fst . rootJ method prec iters f f' . toList
    
    f = toList . (\c -> tangent obc c - p) . fromList
    f' = map toList . tangent' obc . fromList
  
  firstShot <- inv
  pure . snd . foldr1 customMin $ map (residual . firstShot) cs
  where
    n = 8
    customMin (r1,v1) (r2,v2) = if r1 < r2 then (r1,v1) else (r2,v2)
    

invTangentDirect :: C -> C -> Reader Config C
invTangentDirect p init = do
  obc <- asks configCurve
  iters <- asks configInvIterations
  prec <- asks configInvPrecision
  let
    invNoJ = do
      method <- asks configInvMethodNoJ
      pure $ root method prec iters f (toList init)
        
    invJ = do
      method <- asks configInvMethod
      pure $ rootJ method prec iters f f' (toList init)

    f = toList . (\c -> tangent obc c - p) . fromList
    f' = map toList . tangent' obc . fromList
  
  inv <- bool invJ invNoJ <$> asks configInvNoJ
  (l,m) <- inv
  pure $ fromList l


invTangent :: C -> Reader Config R
invTangent p = do
  c <- invTangentDirect p =<< invTangentSpreadInv p
  return (realPart c)

reflect :: C -> Reader Config C
reflect p = do
  g <- asks (gamma . configCurve)
  t <- invTangent p
  pure (2 * g t - p)

