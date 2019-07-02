module Main where

import           Lib

main :: IO ()
main = someFunc

doorPrize :: [Int]
doorPrize = [1000, 2000, 3000]

boxPrize :: [Int]
boxPrize = [500, 20000]

totalPrize :: [Int]
totalPrize = pure (*) <*> doorPrize <*> boxPrize

-- Generating the first N prime numbers
primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite twoThroughN
  where
    twoThroughN = [2 .. n]
    composite = pure (*) <*> twoThroughN <*> twoThroughN
    isNotComposite = not . (`elem` composite)

-- Quickly generating large amounts of test data
data User =
  User
    { name    :: String
    , gamerId :: Int
    , score   :: Int
    }
  deriving (Show)

testNames :: [String]
testNames = ["John Smith", "Robert'); DROP TABLE Students;--", "Christina NULL", "Randall Munroe"]

testIds :: [Int]
testIds = [1337, 0123, 999999]

testScores :: [Int]
testScores = [0, 100000, -99999]

testData :: [User]
testData = pure User <*> testNames <*> testIds <*> testScores

