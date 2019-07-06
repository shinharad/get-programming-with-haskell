module Lib where

toInts :: String -> [Int]
toInts = map read . lines
