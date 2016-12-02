module Area where

import Numeric.LinearAlgebra

wedge :: C -> C -> R
wedge (x1 :+ x2) (y1 :+ y2) = x2 * y1 - y2 * x1

area :: [C] -> R
area pts@(p:pts') =
  (/2) . foldr (+) 0
  $ zipWith wedge pts (pts' ++ [p])
                    
