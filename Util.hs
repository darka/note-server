module Util where

import qualified Data.Char as Char

decapitalize :: String -> String
decapitalize (x:xs) = Char.toLower x : xs
decapitalize [] = []
