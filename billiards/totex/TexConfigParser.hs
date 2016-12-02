module TexConfigParser ( genTexConf, parseTexConf, TexConfig(..) ) where

import Numeric.LinearAlgebra
import Control.Monad.State
import System.IO (hPutStrLn,stderr)
import Data.List (break,intercalate)
import Data.Char (isSpace)
import Text.Read (readEither)

data TexConfig =
  TexConfig {
    configSize :: C,
    configOffset :: C,
    configScale :: C
  }

defaultTexConfig :: TexConfig
defaultTexConfig = TexConfig (150 :+100) (60 :+ 60) (30 :+ 0)

genTexConf :: Maybe FilePath -> IO TexConfig
genTexConf mpath = do
  conf <- case mpath of
    Just path -> readFile path
    Nothing -> return ""
  parseTexConf conf

parseTexConf :: String -> IO TexConfig
parseTexConf str = 
  let
    opts = preProcess str
    (conf,warnings) = runState (foldr (>=>) (return) (map modConf opts) defaultTexConfig) []
  in do
    foldr (>>) (return ()) (map (hPutStrLn stderr) warnings)
    return conf
  where
    relevant s = s /= "" && head s /= '#'
    discardSpaces = filter (not . isSpace)
    preProcess :: String -> [String]
    preProcess = filter relevant . map discardSpaces . lines
    

modConf :: String -> TexConfig -> State [String] TexConfig
modConf s conf = 
  let
    update :: (a -> TexConfig) -> Either String a -> State [String] TexConfig
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
    "size" -> update (\x -> conf { configSize = x }) (readEither value)
    "scale" -> update (\x -> conf { configScale = x }) (readEither value)
    "offset" -> update (\x -> conf { configOffset = x }) (readEither value)
    {- convenience aliases -}
    "width" ->
      let (_ :+ height) = configSize conf
      in update (\x -> conf { configSize = (x :+ height) }) (readEither value)
    "height" ->
      let (width :+ _) = configSize conf
      in update (\x -> conf { configSize = (width :+ x) }) (readEither value)
    "stretch" -> update (\x -> conf { configScale = (x :+ 0) }) (readEither value)
    _ ->
      let warning = "Warning: Unknown option \"" ++ s ++ "\""
      in modify (warning:) >> return conf



