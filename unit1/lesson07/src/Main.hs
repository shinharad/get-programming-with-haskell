module Main where

main :: IO ()
main = return ()

myGCD a b =
  if remainder == 0
    then b
    else myGCD b remainder
  where
    remainder = a `mod` b

sayAmount n =
  case n of
    1 -> "one"
    2 -> "two"
    n -> "a bunch"

sayAmount2 1 = "one"
sayAmount2 2 = "two"
sayAmount2 n = "a bunch"

isEmpty [] = True
isEmpty _  = False

myHead (x:xs) = x
myHead []     = error "No head for empty list"
