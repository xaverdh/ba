module DemoConfigParser ( genDemoConf, parseDemoConf, DemoConfig(..) ) where

import Numeric.LinearAlgebra
import Control.Monad.State
import System.IO (hPutStrLn,stderr)
import Data.List (break,intercalate)
import Data.Char (isSpace)
import Text.Read (readEither)

data DemoConfig =
  DemoConfig {
    configSize :: C,
    configOffset :: C,
    configScale :: C
  }

defaultDemoConfig :: DemoConfig
defaultDemoConfig = DemoConfig (1000 :+ 1000) (600 :+ 400) (100 :+ 0)

genDemoConf :: Maybe FilePath -> IO DemoConfig
genDemoConf mpath = do
  conf <- case mpath of
    Just path -> readFile path
    Nothing -> return ""
  parseDemoConf conf

parseDemoConf :: String -> IO DemoConfig
parseDemoConf str = 
  let
    opts = preProcess str
    (conf,warnings) = runState (foldr (>=>) (return) (map modConf opts) defaultDemoConfig) []
  in do
    foldr (>>) (return ()) (map (hPutStrLn stderr) warnings)
    return conf
  where
    relevant s = s /= "" && head s /= '#'
    discardSpaces = filter (not . isSpace)
    preProcess :: String -> [String]
    preProcess = filter relevant . map discardSpaces . lines
    

modConf :: String -> DemoConfig -> State [String] DemoConfig
modConf s conf = 
  let
    update :: (a -> DemoConfig) -> Either String a -> State [String] DemoConfig
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
    "offset" -> update (\x -> conf { configOffset = x }) (readEither value)
    "scale" -> update (\x -> conf { configScale = x }) (readEither value)    
    "size" -> update (\x -> conf { configSize = x }) (readEither value)
    {- convenience aliases -}
    "stretch" -> update (\x -> conf { configScale = (x :+ 0) }) (readEither value)
    "width" ->
      let (_ :+ height) = configSize conf
      in update (\x -> conf { configSize = (x :+ height) }) (readEither value)
    "height" ->
      let (width :+ _) = configSize conf
      in update (\x -> conf { configSize = (width :+ x) }) (readEither value)
    _ ->
      let warning = "Warning: Unknown option \"" ++ s ++ "\""
      in modify (warning:) >> return conf


