module Texpic.Util where

import Data.Monoid
import Numeric.LinearAlgebra hiding ((<>))
import Texpic.LatexPaint
import Texpic.Color

circle :: C -> Double -> Integer -> Paintable C
circle p r n =
  Curve [ p + g t | t <- points ]
  where
    l = 2 * pi
    points = map (*(l / fromInteger n)) (map fromInteger [1..n])
    g = \t -> (r * cos t) :+ (r * sin t)

grid :: C -> C -> C -> Paintable C
grid p wh1 wh2 = 
  Line (p + pr1 wh1) (p - pr1 wh2)
  <> Line (p + pr2 wh1) (p - pr2 wh2)
  where
    pr1 (x :+ y) = (x :+ 0)
    pr2 (x :+ y) = (0 :+ y)

polygon :: [a] -> Paintable a
polygon a's = mconcat $ zipWith Line a's (tail a's)

withColor :: String -> Paintable C -> Paintable C
withColor = MetaColor . fromString

thick :: Paintable C -> Paintable C
thick = MetaThick

