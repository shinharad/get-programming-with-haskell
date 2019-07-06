module Lib where

import           Control.Monad

-- The guard function
powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = do
  value <- [1 .. n]
  let powersOfTwo = 2 ^ value
  let powersOfThree = 3 ^ value
  return (powersOfTwo, powersOfThree)

allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = do
  evenValue <- [2,4 .. n]
  oddValue <- [1,3 .. n]
  return (evenValue, oddValue)

evensGuard :: Int -> [Int]
evensGuard n = do
  value <- [1 .. n]
  guard (even value)
  return value

-- LIST COMPREHENSIONS
powersOfTwoAndThree' :: Int -> [(Int, Int)]
powersOfTwoAndThree' n =
  [(powersOfTwo, powersOfThree) | value <- [1 .. n], let powersOfTwo = 2 ^ value, let powersOfThree = 3 ^ value]

allEvenOdds' :: Int -> [(Int, Int)]
allEvenOdds' n = [(evenValue, oddValue) | evenValue <- [2,4 .. n], oddValue <- [1,3 .. n]]

evensGuard' :: Int -> [Int]
evensGuard' n = [value | value <- [1 .. n], even value]
