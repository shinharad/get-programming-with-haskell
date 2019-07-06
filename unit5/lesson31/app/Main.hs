module Main where

import           Lib

main :: IO ()
main = do
  name <- getLine
  let statement = helloPerson name
  putStrLn statement
