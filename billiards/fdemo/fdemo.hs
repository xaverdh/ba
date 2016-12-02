{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import Numeric.LinearAlgebra
import Control.Monad.Reader

import Scene (mkScene)
import HtmlPaint
import DemoConfigParser
import ConfigParser

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Data.ByteString.Builder (toLazyByteString,stringUtf8)

import Data.Time.Clock

genHtmlDoc :: DemoConfig -> [Paintable C] -> Int -> IO Html
genHtmlDoc confD scene i = do
  return $ render (configSize confD) (transform confD) (take i scene)
  where
    transform :: DemoConfig -> C -> C
    transform (DemoConfig { configOffset = offset, configScale = scale })
      = (+offset) . (*scale)


mkApp confD scene time req resp = do
  curTime <- getCurrentTime
  Html doc <- genHtmlDoc confD scene $ round (diffUTCTime curTime time)
  {- required if computation takes a long time -}
  pauseTimeout req
  resp $ responseLBS status200 [("Content-Type","text/html; charset=utf-8"),("Refresh","1")] (toLazyByteString . stringUtf8 $ doc)
  -- resp $ responseLBS status200 [("Content-Type","text/html; charset=utf-8")] (toLazyByteString . stringUtf8 $ doc)


main = do
  args <- getArgs
  let (path1,path2) = extract args
  
  confD <- genDemoConf path2
  confS <- genConf path1
  let scene = runReader mkScene confS
  
  time <- getCurrentTime
  runSettings (setup defaultSettings) (mkApp confD scene time)
  where
    setup =
      setServerName "Outer Billiard"
      . setPort 3000
    
    extract :: [String] -> (Maybe FilePath,Maybe FilePath)
    extract args = case args of
      [s1,s2] -> (Just s1,Just s2)
      [s] -> (Just s,Nothing)
      [] -> (Nothing,Nothing)
    




