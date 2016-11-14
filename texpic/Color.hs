module TexPic.Color (
  Color(..)
  ,fromString
) where

import Data.Maybe (mapMaybe)
import Data.Word (Word8)
import Data.String

instance IsString Color where
  fromString = read

data Color =
  Color String
  | RGB { red :: Word8, green :: Word8, blue :: Word8 }
  deriving (Eq,Ord,Show)

colors :: [String]
colors = ["red","green","blue","white","gray","black","cyan","orange","yellow","purple","magenta"]
    
instance Read Color where
  readsPrec _ s = case s of
    'r':'g':'b':rest ->
      case reads rest of
        ((r,g,b),s):_ ->
          if validWord8 r && validWord8 g && validWord8 b
          then [(RGB (fromIntegral r) (fromIntegral g) (fromIntegral b),s)]
          else []
        _ -> []
    _ ->
      case mapMaybe (splitOff s) colors of
        [] -> []
        [(s1,s2)] -> [(Color s1,s2)]
        _ -> []
    where
      validWord8 :: Int -> Bool
      validWord8 i = 0 <= i && i <= 255
  
      {- tries to split off s2 of s1 -}
      splitOff :: String -> String -> Maybe (String,String)
      splitOff s1 s2 =  
        maybe Nothing (\s -> Just (s2,s)) (remains s1 s2)
  
      {- checks if s1 contains s2, returns the remainer if True, otherwise Nothing -}
      remains :: String -> String -> Maybe String
      remains s1 s2 = 
        case (s1,s2) of
          (x:xs,y:ys) ->
            if x == y
            then remains xs ys
            else Nothing
          ([],[]) -> Just []
          (x:xs,[]) -> Just (x:xs)
          ([],_:_) -> Nothing
      
