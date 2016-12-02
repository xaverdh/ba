module HtmlPaint ( Color(..), Html(..), Paintable(..), render ) where

import Paintable
import Data.List (intercalate)
import Numeric.LinearAlgebra hiding (conv)

data PathData = PathData String

conv :: C -> PathData
conv (x :+ y) =
  PathData
    $ (show $ round x)
    ++ " "
    ++ (show $ round y)

{-
  Rendering
-}


render :: C -> (C-> C) -> [Paintable C] -> Html
render size transform =  
  htmlWrap [mkHtmlAttr "style" ("background-color:grey")]
  . mkSvgTag size 
  . joinHtml 
  . map (renderSingle transform)

htmlWrap :: [HtmlAttr] -> Html -> Html
htmlWrap bodyattrs content =
  joinHtml [
      Html "<!DOCTYPE html>",
      mkHtmlTag "html" [] (
        mkHtmlTag "body" bodyattrs content
      )
    ]
 
renderSingle :: (C-> C) -> Paintable C -> Html
renderSingle transform ptble
  = case ptble of
      Curve pts color -> curve color (map transform pts)
      Line p1 p2 color -> line color (transform p1) (transform p2)
      Arc p1 p2 p3 color -> arc color (transform p1) (transform p2) (transform p3)
      {- Warning: Circle will not get tranformed properly under some mappings. -}
      Circle p r color -> circle color (transform p) (compRadius p r)
  where
    compRadius p r = magnitude (transform (p + (r :+ 0)) - transform p)


mkSvgTag :: C -> Html -> Html
mkSvgTag (width :+ height) content =
  mkHtmlTag "svg" [
    mkHtmlAttr "width" (show . round $ width),
    mkHtmlAttr "height" (show . round $ height)
    ] content


{-
  PathData
-}

forgetfulPathData :: PathData -> String
forgetfulPathData (PathData s) = s

joinPathData :: [PathData] -> PathData
joinPathData = PathData . intercalate " " . map forgetfulPathData

args :: [C] -> PathData
args pts = joinPathData $ map conv pts

mkArc :: C -> C -> C -> PathData
mkArc p1 p2 p3 =
  joinPathData [ PathData "M", conv p1, PathData "Q", args [p2,p3] ]

mkCurve :: [C] -> PathData
mkCurve pts =
  case pts of
    (p1:p2:rest) -> joinPathData $ zipWith3 mkArc pts (p2:rest) rest
    _ -> PathData ""

mkLine :: C -> C -> PathData
mkLine pt v = joinPathData [ PathData "M", conv pt, PathData "L", conv v ]


{- Html -}


data Html = Html String
data HtmlAttr = HtmlAttr String

forgetfulHtml :: Html -> String
forgetfulHtml (Html s) = s

forgetfulHtmlAttr :: HtmlAttr -> String
forgetfulHtmlAttr (HtmlAttr s) = s

joinHtml = Html . intercalate "\n" . map forgetfulHtml

mkHtmlAttr :: String -> String -> HtmlAttr
mkHtmlAttr name value = HtmlAttr $ name ++ "=\"" ++ value ++ "\""

mkHtmlTag :: String -> [HtmlAttr] -> Html -> Html
mkHtmlTag name attrs (Html content) = Html
  $ intercalate " "
  $ ['<':name] ++ map forgetfulHtmlAttr attrs ++ [">\n" ++ content ++"</" ++ name ++ ">"]

mkSimpleHtmlTag :: String -> [HtmlAttr] -> Html
mkSimpleHtmlTag name attrs = Html
  $ intercalate " "
  $ ["<" ++ name] ++ map forgetfulHtmlAttr attrs ++ ["/>"]

mkPathTag :: Color -> PathData -> Html
mkPathTag color (PathData content) =
  mkSimpleHtmlTag "path" [
    mkHtmlAttr "d" content,
    mkHtmlAttr "stroke" (showColor color)
    ]
  where
    showColor col = case col of
      Color plain -> plain
      RGB { red = r, green = g, blue = b } ->
        "rgb("
        ++ show r
        ++ ","
        ++ show g
        ++ ","
        ++ show b
        ++ ")"

arc :: Color -> C -> C -> C -> Html
arc color p1 p2 p3 = mkPathTag color (mkArc p1 p2 p3)

curve :: Color -> [C] -> Html
curve color pts = mkPathTag color (mkCurve pts)

line :: Color -> C -> C -> Html
line color p1 p2 = mkPathTag color (mkLine p1 p2)

circle :: Color -> C -> Double ->  Html
circle (Color color) pt r =
  mkSimpleHtmlTag "circle" [
    mkHtmlAttr "cx" . show . round . realPart $ pt,
    mkHtmlAttr "cy" . show . round . imagPart $ pt,
    mkHtmlAttr "r" $ show (round r),
    mkHtmlAttr "stroke" color,
    mkHtmlAttr "fill" $ color
    ]


