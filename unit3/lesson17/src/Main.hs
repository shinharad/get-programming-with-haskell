module Main where

import           Data.List

main :: IO ()
main = return ()

myLast :: [a] -> a
myLast = head . reverse

myMin :: Ord a => [a] -> a
myMin = head . sort

myMax :: Ord a => [a] -> a
myMax = myLast . sort

myAll :: (a -> Bool) -> [a] -> Bool
myAll testFunc = (foldr (&&) True) . (map testFunc)

myAny :: (a -> Bool) -> [a] -> Bool
myAny testFunc = (foldr (||) False) . (map testFunc)

-- 17.2.1. The Color Semigroup
data Color
  = Red
  | Yellow
  | Blue
  | Green
  | Purple
  | Orange
  | Brown
  deriving (Show, Eq)

instance Semigroup Color where
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Orange
  (<>) a b
    | a == b = a
    | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
    | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
    | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
    | otherwise = Brown

main1 = do
  print $ Red <> Yellow
    -- Orange
  print $ Red <> Blue
    -- Purple
  print $ Green <> Purple
    -- Brown

main2 = do
  print $ (Green <> Blue) <> Yellow
    -- Green
  print $ Green <> (Blue <> Yellow)
    -- Green

-- ----------------------------------------------
--  17.3. COMPOSING WITH IDENTITY: MONOIDS
-- 17.3.3. Practical Monoids—building probability tables
type Events = [String]

type Probs = [Double]

data PTable =
  PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
  where
    totalProbs = sum probs
    normalizedProbs = map (\x -> x / totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
  show (PTable events probs) = mconcat pairs
    where
      pairs = zipWith showPair events probs

main3 = print $ createPTable ["heads", "tails"] [0.5, 0.5]
  -- heads|0.5
  -- tails|0.5

-- ----------------------------------------------
-- The cartCombine function for the Cartesian product of lists
cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where
    nToAdd = length l2
    repeatedL1 = map (take nToAdd . repeat) l1
    newL1 = mconcat repeatedL1
    cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartCombine combiner e1 e2
  where
    combiner = (\x y -> mconcat [x, "-", y])

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = cartCombine (*) p1 p2

instance Semigroup PTable where
  (<>) ptable1 (PTable [] []) = ptable1
  (<>) (PTable [] []) ptable2 = ptable2
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
    where
      newEvents = combineEvents e1 e2
      newProbs = combineProbs p1 p2

instance Monoid PTable where
  mempty = PTable [] []
  mappend = (<>)

coin :: PTable
coin = createPTable ["heads", "tails"] [0.5, 0.5]

spinner :: PTable
spinner = createPTable ["red", "blue", "green"] [0.1, 0.2, 0.7]

main4 = print $ coin <> spinner
  -- heads-red|5.0e-2
  -- heads-blue|0.1
  -- heads-green|0.35
  -- tails-red|5.0e-2
  -- tails-blue|0.1
  -- tails-green|0.35

main5 = print $ mconcat [coin, coin, coin]
  -- heads-heads-heads|0.125
  -- heads-heads-tails|0.125
  -- heads-tails-heads|0.125
  -- heads-tails-tails|0.125
  -- tails-heads-heads|0.125
  -- tails-heads-tails|0.125
  -- tails-tails-heads|0.125
  -- tails-tails-tails|0.125
