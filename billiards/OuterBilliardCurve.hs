module OuterBilliardCurve where

import Numeric.LinearAlgebra
import Data.Fixed (mod')

data OuterBilliardCurve =
  OBC {
    gamma :: R -> C,
    gamma' :: R -> C,
    gamma'' :: R -> C,
    lenParam :: R
  }


mkEllipse :: R -> R -> OuterBilliardCurve
mkEllipse a b =
  OBC {
    gamma = \t -> a * cos t :+ b * sin t,
    gamma' = \t -> (-a*sin t) :+ (b*cos t),
    gamma'' = \t ->  (-a*cos t) :+ (-b*sin t),
    lenParam = 2*pi
  }

mkCircle :: R -> OuterBilliardCurve
mkCircle r = mkEllipse r r


rect :: OuterBilliardCurve
rect = 
  OBC {
    gamma = compWise
      (zipWith
        (\c v -> (+c) . smult v)
        [ 0 :+ 1, 1 :+ 0, 0 :+ (-1), (-1) :+ 0 ]
        [ 1 :+ (-1), (-1) :+ (-1), (-1) :+ 1, 1 :+ 1 ]
        ) . prep,
    gamma' = compWise (map const [1 :+ (-1),(-1) :+ (-1),(-1) :+ 1,1 :+ 1]) . prep,
    gamma'' = const 0,
    lenParam = 4
  }
  where
    prep t = t `mod'` 4
    
    smult :: C -> R -> C
    smult c l = (l :+ 0) * c

    compWise [f1,f2,f3,f4] t
      | t < 1 = f1 t
      | t < 2 = f2 (t-1)
      | t < 3 = f3 (t-2)
      | otherwise = f4 (t-3)


halfCircle :: OuterBilliardCurve
halfCircle =  
  OBC {
    gamma =
      compWise
      (\t -> cos t :+ sin t)
      (\t -> (2*t/pi-3) :+ 0),
    gamma' =
      compWise
      (\t -> (-sin t) :+ cos t)
      (\t -> 2/pi :+ 0),
    gamma'' = 
      compWise
      (\t -> (-cos t) :+ (-sin t))
      (\t -> 0),
    lenParam = 2*pi
  }
  where
    compWise f g t =
      let t' =  t `mod'` (2*pi)
      in if t' < pi then f t' else g t'

toTheFourth :: OuterBilliardCurve
toTheFourth = 
  OBC {
    gamma = compWise
      (\t -> (-1 + (t-1) ** 4) :+ (t-1) )
      (\t -> (1 -(3-t) ** 4) :+ (3-t) ),
    gamma' = compWise
      (\t -> (4 * (t-1) ** 3) :+ 1 )
      (\t -> (4 * (3-t) ** 3) :+ (-1) ),
    gamma'' = compWise
      (\t -> (12 * (t-1) ** 2) :+ 0 )
      (\t -> (-12 * (3-t) ** 2) :+ 0 ),
    lenParam = 4
  }
  where
    compWise f g t = 
      let t' =  t `mod'` 4
      in if t' < 2 then f t' else g t'



