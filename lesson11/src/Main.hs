module Main where

main :: IO ()
main = do
  putStrLn "hello world"

x :: Int
x = 2

y :: Integer
y = 2

letter :: Char
letter = 'a'

interestRate :: Double
interestRate = 0.375

isFun :: Bool
isFun = True

values :: [Int]
values = [1,2,3]

testScores :: [Double]
testScores = [0.99,0.7,0.8]

letters :: [Char]
letters = ['a','b','c']

aPet :: [Char]
aPet = "cat"

anotherPet :: String
anotherPet = "dog"

ageAndHeight :: (Int, Int)
ageAndHeight = (34, 74)

half :: Int -> Double
half n = (fromIntegral n) / 2

halve :: Integer -> Integer
halve value = value `div` 2

makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number, street, town)

makeTriple :: a -> b -> c -> (a,b,c)
makeTriple x y z = (x,y,z)