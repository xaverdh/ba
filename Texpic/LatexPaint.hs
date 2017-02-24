{-# language LambdaCase #-}
module Texpic.LatexPaint (
  Color(..)
  ,Paintable(..)
  ,render
) where

import Data.String
import Numeric (showFFloat)
import Numeric.LinearAlgebra hiding (conv)
import Texpic.Paintable
import Texpic.Color
import Data.Functor.Foldable


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


render :: C -> Paintable C -> String
render size ptble =
  let
    beginPicture = "\\begin{picture}" ++ conv size
    endPicture = "\\end{picture}"
    content = paint ptble
  in unlines [beginPicture,content,endPicture]

paint :: Paintable C -> String
paint = cata $ \case
    EmptyF -> ""
    ComposedF x1 x2 -> unlines' [x1,x2]
    CurveF pts -> mkCurve pts
    LineF p1 p2 -> mkLine p1 p2
    ArcF p1 p2 p3 -> mkArc p1 p2 p3
    TexF p s -> mkTexCode p s
    MetaThickF x -> unlines' ["\\thicklines",x,"\\thinlines"]
    MetaColorF col x -> unlines' [mkColor col,x,mkColor $ fromString "black"]
  where
    unlines' = unlines . filter (/="")

mkTexCode :: C -> String -> String
mkTexCode p s = unlines ["\\put",conv p,"{",s,"}"]

mkArc :: C -> C -> C -> String
mkArc p1 p2 p3 =
  "\\qbezier" ++ conv p1 ++ conv p2 ++ conv p3

mkColor :: Color -> String
mkColor = \case
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

mkCurve :: [C] -> String
mkCurve pts =
  case pts of
    (p1:p2:rest) -> composeCurve pts (p2:rest) rest
    _ -> ""
  where
    composeCurve :: [C] -> [C] -> [C] -> String
    composeCurve pts1 pts2 pts3 =
      unlines $ zipWith3 mkArc pts1 pts2 pts3

mkLine :: C -> C -> String
mkLine p1 p2 = mkArc p1 (avg p1 p2) p2
  where
    avg a b = (a + b)/2


