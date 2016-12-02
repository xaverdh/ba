{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import System.Environment (getArgs)
import Numeric.LinearAlgebra
import Control.Monad.Reader

import Scene (mkScene)
import LatexPaint
import TexConfigParser
import ConfigParser


genLaTexDoc :: Maybe FilePath -> Maybe FilePath -> IO String
genLaTexDoc mpath1 mpath2 = do
  confD <- genTexConf mpath2
  confS <- genConf mpath1
  let scene = runReader mkScene confS
  return $ render (configSize confD) (transform confD) $ take (configIterations confS) scene
  where
    transform :: TexConfig -> C -> C
    transform (TexConfig { configOffset = offset, configScale = scale })
      = (+offset) . (*scale)

main = do
  args <- getArgs
  let (path1,path2) = extract args
  genLaTexDoc path1 path2 >>= putStrLn
  where
    extract :: [String] -> (Maybe FilePath,Maybe FilePath)
    extract args = case args of
      [s1,s2] -> (Just s1,Just s2)
      [s] -> (Just s,Nothing)
      [] -> (Nothing,Nothing)





