module Lib where

maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x

--
-- GHCi> (:) <$> maybeHead [1,2,3] <*> Just [9]
-- Just [1,9]
-- GHCi> (:) <$> maybeHead [1,2,3] <*> Just []
-- Just [1]
-- GHCi> (:) <$> maybeHead [] <*> Just []
-- Nothing
--
-- GHCi> myTakeSafer 3 (Just [1,2,3])
-- Just [1,2,3]
-- GHCi> myTakeSafer 6 (Just [1,2,3])
-- Nothing
--
myTakeSafer :: Int -> Maybe [a] -> Maybe [a]
myTakeSafer 0 _ = Just []
myTakeSafer n (Just xs) = (:) <$> maybeHead xs <*> myTakeSafer (n - 1) (Just (tail xs))

primes :: [Int]
primes = [2, 3, 5, 7]

maxN :: Int
maxN = 10

isPrime :: Int -> Maybe Bool
isPrime n
  | n < 2 = Nothing
  | n > maxN = Nothing
  | otherwise = Just (n `elem` primes)

eitherHead :: [a] -> Either String a
eitherHead []     = Left "There is no head because the list is empty"
eitherHead (x:xs) = Right x

intExample :: [Int]
intExample = [1, 2, 3]

intExampleEmpty :: [Int]
intExampleEmpty = []

charExample :: [Char]
charExample = "cat"

charExampleEmpty :: [Char]
charExampleEmpty = ""

isPrime' :: Int -> Either PrimeError Bool
isPrime' n
  | n < 2 = Left InvalidValue
  | n > maxN = Left TooLarge
  | otherwise = Right (n `elem` primes)

data PrimeError
  = TooLarge
  | InvalidValue

instance Show PrimeError where
  show TooLarge     = "Value exceed max bound"
  show InvalidValue = "Value is not a valid candidate for prime checking"

displayResult :: Either PrimeError Bool -> String
displayResult (Right True)      = "It's prime"
displayResult (Right False)     = "It's composite"
displayResult (Left primeError) = show primeError

