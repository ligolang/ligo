module Util (prepareField) where

import Data.Char (toLower)

prepareField :: Int -> String -> String
prepareField n = lowercaseInitial . drop n
  where
    lowercaseInitial :: String -> String
    lowercaseInitial [] = []
    lowercaseInitial (c:s) = toLower c : s
