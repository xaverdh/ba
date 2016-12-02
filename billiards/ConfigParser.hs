module ConfigParser ( parseConf, genConf, Config(..) ) where

import Numeric.LinearAlgebra
import Numeric.GSL.Root
import Control.Monad.State
import System.IO (hPutStrLn,stderr)
import Data.List (break,intercalate)
import Data.Char (isSpace)
import Text.Read (readEither)

import OuterBilliardCurve
import Color

instance Read RootMethodJ where
  readsPrec _ s = case s of
    'H':'y':'b':'r':'i':'d':'s':'J':rest -> [(HybridsJ,rest)]
    'H':'y':'b':'r':'i':'d':'J':rest -> [(HybridJ,rest)]
    'N':'e':'w':'t':'o':'n':rest -> [(Newton,rest)]
    'G':'N':'e':'w':'t':'o':'n':rest -> [(GNewton,rest)]
    _ -> []

instance Read RootMethod where
  readsPrec _ s = case s of
    'H':'y':'b':'r':'i':'d':'s':rest -> [(Hybrids,rest)]
    'H':'y':'b':'r':'i':'d':rest -> [(Hybrid,rest)]
    'D':'N':'e':'w':'t':'o':'n':rest -> [(DNewton,rest)]
    'B':'r':'o':'y':'d':'e':'n':rest -> [(Broyden,rest)]
    _ -> []

data Config =
  Config {
    configStart :: C,
    configIterations :: Int,
    configLineColor :: Color,
    configCurveColor :: Color,
    configStartColor :: Color,
    configEndColor :: Color,
    configCurve :: OuterBilliardCurve,
    configInvFirstShotSimple :: Bool,
    configInvFirstShotIterations :: Int,
    configInvFirstShotPrecision :: Double,
    configInvFirstShotMethod :: RootMethodJ,
    configInvFirstShotNoJ :: Bool,
    configInvFirstShotMethodNoJ :: RootMethod,
    configInvIterations :: Int,
    configInvPrecision :: Double,
    configInvMethod :: RootMethodJ,
    configInvNoJ :: Bool,
    configInvMethodNoJ :: RootMethod
  }

defaultConfig :: Config
defaultConfig =
  Config
  (1.5 :+ 1.5)
  (3)
  (read "green") 
  (read "blue")
  (read "black")
  (read "red")
  (mkCircle 1)
  False
  (10)
  (1e2)
  HybridsJ
  False
  Hybrids
  (1000)
  (1e-14)
  HybridsJ
  False
  Hybrids


genConf :: Maybe FilePath -> IO Config
genConf mpath =
  case mpath of
    Just path -> readFile path >>= parseConf
    Nothing -> return defaultConfig

parseConf :: String -> IO Config
parseConf str = 
  let
    opts = preProcess str
    (conf,warnings) = runState (foldr (>=>) (return) (map modConf opts) defaultConfig) []
  in do
    foldr (>>) (return ()) (map (hPutStrLn stderr) warnings)
    return conf
  where
    relevant s = s /= "" && head s /= '#'
    discardSpaces = filter (not . isSpace)
    preProcess :: String -> [String]
    preProcess = filter relevant . map discardSpaces . lines
    

modConf :: String -> Config -> State [String] Config
modConf s conf = 
  let
    update :: (a -> Config) -> Either String a -> State [String] Config
    update setter err_or_value = 
      case err_or_value of
        Right value -> return (setter value)
        Left err -> 
          let warning = "Warning: Failed to parse option \"" ++ s ++ "\"\n" ++ err
          in modify (warning:) >> return conf

    (name,value') = break (==':') s
    value = case value' of
      "" -> ""
      _:rest -> rest
  in case name of
    "start" -> update (\x -> conf { configStart = x }) (readEither value)
    "iterations" -> update (\x -> conf { configIterations = x }) (readEither value)
    "linecolor" -> update (\x -> conf { configLineColor = x }) (readEither value)
    "curvecolor" -> update (\x -> conf { configCurveColor = x }) (readEither value)
    "startcolor" -> update (\x -> conf { configStartColor = x }) (readEither value)
    "endcolor" -> update (\x -> conf { configEndColor = x }) (readEither value)
    "curve" -> update (\x -> conf { configCurve = x }) (readCurve value)
    "firstshot-simple" -> update (\x -> conf { configInvFirstShotSimple = x }) (readEither value)
    "firstshot-iterations" -> update (\x -> conf { configInvFirstShotIterations = x }) (readEither value)
    "firstshot-precision" -> update (\x -> conf { configInvFirstShotPrecision = x }) (readEither value)
    "firstshot-noj" -> update (\x -> conf { configInvFirstShotNoJ = x }) (readEither value)
    "firstshot-method" -> update (\x -> conf { configInvFirstShotMethod = x }) (readEither value)
    "firstshot-method-noj" -> update (\x -> conf { configInvFirstShotMethodNoJ = x }) (readEither value)
    "inv-iterations" -> update (\x -> conf { configInvIterations = x }) (readEither value)
    "inv-precision" -> update (\x -> conf { configInvPrecision = x }) (readEither value)
    "inv-noj" -> update (\x -> conf { configInvNoJ = x }) (readEither value)
    "inv-method" -> update (\x -> conf { configInvMethod = x }) (readEither value)
    "inv-method-noj" -> update (\x -> conf { configInvMethodNoJ = x }) (readEither value)
    _ ->
      let warning = "Warning: Unknown option \"" ++ s ++ "\""
      in modify (warning:) >> return conf


readCurve :: String -> Either String OuterBilliardCurve
readCurve value = 
  case value of
    "circle" -> Right (mkCircle 1)
    "ellipse" -> Right (mkEllipse 2 1)
    "tothe4th" -> Right (toTheFourth)
    "rect" -> Right (rect)
    "halfCircle" -> Right (halfCircle)
    _ -> Left
      $ intercalate "\n" [
        "expected one of:",
        "circle",
        "ellipse",
        "tothe4th",
        "rect",
        "halfCircle"
        ]
      
