module Main where

main :: IO ()
main = return ()

-- ----------------------------------------------
x :: Int
x = 2

main1 = do
  print $ x * 2000
    -- 4000
  print $ x ^ 2000
    -- 0

-- ----------------------------------------------
y :: Integer
y = 2

main2 = do
  print $ y * 2000
    -- 4000
  print $ y ^ 2000
    -- 114813069527425452423283320117768198402231770208869520047764273682576626139237031385665948631650626991844596463898746277344711896086305533142593135616665318539129989145312280000688779148240044871428926990063486244781615463646388363947317026040466353970904996558162398808944629605623311649536164221970332681344168908984458505602379484807914058900934776500429002716706625830522008132236281291761267883317206598995396418127021779858404042159853183251540889433902091920554957783589672039160081957216630582755380425583726015528348786419432054508915275783882625175435528800822842770817965453762184851149029376

-- ----------------------------------------------
letter :: Char
letter = 'a'

interestRate :: Double
interestRate = 0.375

isFun :: Bool
isFun = True

values :: [Int]
values = [1, 2, 3]

testScores :: [Double]
testScores = [0.99, 0.7, 0.8]

letters :: [Char]
letters = ['a', 'b', 'c']

main3 = do
  print $ letters == "abc"
    -- True

-- ----------------------------------------------
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

makeTriple :: a -> b -> c -> (a, b, c)
makeTriple x y z = (x, y, z)

main4 = print $ makeAddress 123 "Happy St" "Haskell Town"
  -- (123,"Happy St","Haskell Town")
