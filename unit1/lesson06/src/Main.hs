module Main where

main :: IO ()
main = do
  putStrLn "hello world"

assignToGroups n aList = zip groups aList
  where groups = cycle [1..n]

