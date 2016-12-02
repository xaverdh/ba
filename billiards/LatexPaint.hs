module LatexPaint ( Color(..), Paintable(..), render ) where
import Paintable
import Data.List (intercalate)
import Numeric (showFFloat)
import Numeric.LinearAlgebra hiding (conv)


conv :: C -> String
conv (x :+ y) =
  "("
  ++ (truncShow x)
  ++ ","
  ++ (truncShow y)
  ++ ")"
  where
    truncShow :: Double -> String
    truncShow d = showFFloat (Just 8) d ""
    
render :: C -> (C -> C) -> [Paintable C] -> String
render size transform ptbles =
  let
    beginPicture = "\\begin{picture}" ++ conv size
    endPicture = "\\end{picture}"
    contentLines = map (renderSingle transform) ptbles
  in intercalate "\n" ([beginPicture] ++ contentLines ++ [endPicture])

renderSingle :: (C -> C) -> Paintable C -> String
renderSingle transform ptble =
  case ptble of
    Curve pts color -> curve color (map transform pts)
    Line p1 p2 color -> line color (transform p1) (transform p2)
    Arc p1 p2 p3 color -> arc color (transform p1) (transform p2) (transform p3)
    {- Warning:
      Circle will not get transformed properly under some mappings.
      -}
    Circle p r color -> circle color (transform p) (compRadius p r)
  where
    compRadius p r = magnitude (transform (p + (r :+ 0)) - transform p)

mkArc :: C -> C -> C -> String
mkArc p1 p2 p3 =
  "\\qbezier" ++ conv p1 ++ conv p2 ++ conv p3

mkColorLine :: Color -> String
mkColorLine color =
  case color of
    Color plain ->
      "\\color{" ++ plain ++ "}"
    RGB { red = r, green = g, blue = b } ->
      "\\color[RGB]{"
      ++ show r
      ++ ","
      ++ show g
      ++ ","
      ++ show b
      ++ "}"

arc :: Color -> C -> C -> C -> String
arc color p1 p2 p3 = mkColorLine color ++ "\n" ++ mkArc p1 p2 p3

curve :: Color -> [C] -> String
curve color pts =
  case pts of
    (p1:p2:rest) -> composeCurve color pts (p2:rest) rest
    _ -> ""
  where
    composeCurve :: Color -> [C] -> [C] -> [C] -> String
    composeCurve color pts1 pts2 pts3 =
      let
        colorLine = mkColorLine color
        curveLines = zipWith3 mkArc pts1 pts2 pts3
      in intercalate "\n" (colorLine:curveLines)

line :: Color -> C -> C -> String
line color p1 p2 =
  let
    colorLine = mkColorLine color
    lineLine = mkArc p1 (avg p2 p1) p2
  in colorLine ++ "\n" ++ lineLine
  where
    avg p1 p2 = (p1 + p2)/2

circle :: Color -> C -> Double -> String
circle color p r =
  let
    colorLine = mkColorLine color
    circleLine =
      "\\put"
      ++ conv p
      ++ "{\\circle*{"
      {- LaTex wants the diameter here, not the radius, so multiply by 2 -}
      ++ (show (2*r))
      ++ "}}"
  in colorLine ++ "\n" ++ circleLine





