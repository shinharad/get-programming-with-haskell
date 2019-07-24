module Main where

import           Data.List
import qualified Data.Map       as Map
import           Data.Maybe
import           Data.Semigroup

main :: IO ()
main = return ()

file1 :: [(Int, Double)]
file1 = [(1, 200.1), (2, 199.5), (3, 199.4), (4, 198.9), (5, 199.0), (6, 200.2), (9, 200.3), (10, 201.2), (12, 202.9)]

file2 :: [(Int, Double)]
file2 = [(11, 201.6), (12, 201.5), (13, 201.5), (14, 203.5), (15, 204.9), (16, 207.1), (18, 210.5), (20, 208.8)]

file3 :: [(Int, Double)]
file3 = [(10, 201.2), (11, 201.6), (12, 201.5), (13, 201.5), (14, 203.5), (17, 210.5), (24, 215.1), (25, 218.7)]

file4 :: [(Int, Double)]
file4 =
  [ (26, 219.8)
  , (27, 220.5)
  , (28, 223.8)
  , (29, 222.8)
  , (30, 223.8)
  , (31, 221.7)
  , (32, 222.3)
  , (33, 220.8)
  , (34, 219.4)
  , (35, 220.1)
  , (36, 220.6)
  ]

-- The definition of the TS data type
data TS a =
  TS [Int] [Maybe a]

-- createTS to make an easier interface for creating TS types
createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes extendValues
  where
    completeTimes = [minimum times .. maximum times]
    timeValueMap = Map.fromList (zip times values)
    extendValues = map (\v -> Map.lookup v timeValueMap) completeTimes

-- fileToTS to easily convert your file data into TS types
fileToTS :: [(Int, a)] -> TS a
fileToTS tvPairs = createTS times values
  where
    (times, values) = unzip tvPairs

-- showTVPair to render time/value pairs readable
showTVPair :: Show a => Int -> (Maybe a) -> String
showTVPair time (Just value) = mconcat [show time, "|", show value, "\n"]
showTVPair time Nothing      = mconcat [show time, "|NA\n"]

-- Making TS an instance of Show by using zipWith and showTVPair
instance Show a => Show (TS a) where
  show (TS times values) = mconcat rows
    where
      rows = zipWith showTVPair times values

main1 = print $ fileToTS file1
  -- 1|200.1
  -- 2|199.5
  -- 3|199.4
  -- 4|198.9
  -- 5|199.0
  -- 6|200.2
  -- 7|NA
  -- 8|NA
  -- 9|200.3
  -- 10|201.2
  -- 11|NA
  -- 12|202.9

-- ----------------------------------------------
-- Converting all your data files into TS types
ts1 :: TS Double
ts1 = fileToTS file1

ts2 :: TS Double
ts2 = fileToTS file2

ts3 :: TS Double
ts3 = fileToTS file3

ts4 :: TS Double
ts4 = fileToTS file4

-- insertMaybePair, a helper function for inserting (k, Maybe v) pairs
insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_, Nothing)        = myMap
insertMaybePair myMap (key, (Just value)) = Map.insert key value myMap

combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2 = ts2
combineTS ts1 (TS [] []) = ts1
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
  where
    bothTimes = mconcat [t1, t2]
    completeTimes = [minimum bothTimes .. maximum bothTimes]
    tvMap = foldl insertMaybePair Map.empty (zip t1 v1)
    updatedMap = foldl insertMaybePair tvMap (zip t2 v2)
    combinedValues = map (\v -> Map.lookup v updatedMap) completeTimes

-- Making TS an instance of Semigroup
instance Semigroup (TS a) where
  (<>) = combineTS

main2 = print $ ts1 <> ts2
  -- 1|200.1
  -- 2|199.5
  -- 3|199.4
  -- 4|198.9
  -- 5|199.0
  -- 6|200.2
  -- 7|NA
  -- 8|NA
  -- 9|200.3
  -- 10|201.2
  -- 11|201.6
  -- 12|201.5
  -- 13|201.5
  -- 14|203.5
  -- 15|204.9
  -- 16|207.1
  -- 17|NA
  -- 18|210.5
  -- 19|NA
  -- 20|208.8

-- ----------------------------------------------
-- Making TS an instance of Monoid
instance Monoid (TS a) where
  mempty = TS [] []
  mappend = (<>)

main3 = print $ mconcat [ts1, ts2]
  -- 1|200.1
  -- 2|199.5
  -- 3|199.4
  -- 4|198.9
  -- 5|199.0
  -- 6|200.2
  -- 7|NA
  -- 8|NA
  -- 9|200.3
  -- 10|201.2
  -- 11|201.6
  -- 12|201.5
  -- 13|201.5
  -- 14|203.5
  -- 15|204.9
  -- 16|207.1
  -- 17|NA
  -- 18|210.5
  -- 19|NA
  -- 20|208.8

-- ----------------------------------------------
tsAll :: TS Double
tsAll = mconcat [ts1, ts2, ts3, ts4]

-- 20.3. PERFORMING CALCULATIONS ON YOUR TIME SERIES
-- mean to average a list of most number types
mean :: (Real a) => [a] -> Double
mean xs = total / count
  where
    total = (realToFrac . sum) xs
    count = (realToFrac . length) xs

meanTS :: (Real a) => TS a -> Maybe Double
meanTS (TS _ []) = Nothing
meanTS (TS times values) =
  if all (== Nothing) values
    then Nothing
    else Just avg
  where
    justVals = filter isJust values
    cleanVals = map fromJust justVals
    avg = mean cleanVals

main4 = print $ meanTS tsAll
  -- Just 210.5966666666667

-- ----------------------------------------------
-- Listing 20.15. makeTSCompare and useful type synonyms
type CompareFunc a = a -> a -> a

type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare func = newFunc
  where
    newFunc (i1, Nothing) (i2, Nothing) = (i1, Nothing) -- pattern matching.
    newFunc (_, Nothing) (i, val) = (i, val)
    newFunc (i, val) (_, Nothing) = (i, val)
    newFunc (i1, Just val1) (i2, Just val2) =
      if func val1 val2 == val1
        then (i1, Just val1)
        else (i2, Just val2)

main5 = print $ makeTSCompare max (3, Just 200) (4, Just 10)
  -- (3,Just 200)

-- ----------------------------------------------
-- Listing 20.16. compareTS, a generic means of applying comparison functions to TS
compareTS :: Eq a => (a -> a -> a) -> TS a -> Maybe (Int, Maybe a)
compareTS func (TS [] []) = Nothing
compareTS func (TS times values) =
  if all (== Nothing) values
    then Nothing
    else Just best
  where
    pairs = zip times values
    best = foldl (makeTSCompare func) (0, Nothing) pairs

-- Listing 20.17. Trivially creating minTS and maxTS using compareTS
minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max

main6 = do
  print $ minTS tsAll
    -- Just (4,Just 198.9)
  print $ maxTS tsAll
   -- Just (28,Just 223.8)

-- ----------------------------------------------
-- 20.4. TRANSFORMING TIME SERIES
diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair Nothing _         = Nothing
diffPair _ Nothing         = Nothing
diffPair (Just x) (Just y) = Just (x - y)

-- diffTS to take the diff of a TS
diffTS :: Num a => TS a -> TS a
diffTS (TS [] []) = TS [] []
diffTS (TS times values) = TS times (Nothing : diffValues)
  where
    shiftValues = tail values
    diffValues = zipWith diffPair shiftValues values -- valuesの末尾が欠落？

main7 = print $ meanTS (diffTS tsAll)
  -- Just 0.6076923076923071

-- ----------------------------------------------
--movingAverageTS :: (Real a) => TS a -> Int -> TS Double
meanMaybe :: (Real a) => [Maybe a] -> Maybe Double
meanMaybe vals =
  if any (== Nothing) vals
    then Nothing
    else (Just avg)
  where
    avg = mean (map fromJust vals)

-- movingAvg calculates the moving average of a Maybe a list
movingAvg :: (Real a) => [Maybe a] -> Int -> [Maybe Double]
movingAvg [] n = []
movingAvg vals n =
  if length nextVals == n
    then meanMaybe nextVals : movingAvg restVals n
    else []
  where
    nextVals = take n vals
    restVals = tail vals

-- maTS for calculating the moving average of a TS with centering
movingAverageTS :: (Real a) => TS a -> Int -> TS Double
movingAverageTS (TS [] []) n = TS [] []
movingAverageTS (TS times values) n = TS times smoothedValues
  where
    ma = movingAvg values n
    nothings = replicate (n `div` 2) Nothing
    smoothedValues = mconcat [nothings, ma, nothings]
