module Main where

import Lib

import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe

main :: IO ()
main = someFunc

file1 :: [(Int,Double)]
file1 = [ (1, 200.1), (2,  199.5), (3, 199.4)
        , (4, 198.9), (5,  199.0), (6, 200.2)
        , (9, 200.3), (10, 201.2), (12, 202.9)]

file2 :: [(Int,Double)]
file2 = [(11, 201.6), (12, 201.5), (13, 201.5)
        ,(14, 203.5), (15, 204.9), (16, 207.1)
        ,(18, 210.5), (20, 208.8)]

file3 :: [(Int,Double)]
file3 = [(10, 201.2), (11, 201.6), (12, 201.5)
        ,(13, 201.5), (14, 203.5), (17, 210.5)
        ,(24, 215.1), (25, 218.7)]

file4 :: [(Int,Double)]
file4 = [(26, 219.8), (27, 220.5), (28, 223.8)
        ,(29, 222.8), (30, 223.8), (31, 221.7)
        ,(32, 222.3), (33, 220.8), (34, 219.4)
        ,(35, 220.1), (36, 220.6)]

-- The definition of the TS data type
data TS a = TS [Int] [Maybe a]

-- createTS to make an easier interface for creating TS types
createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes extendValues
  where completeTimes = [minimum times .. maximum times]
        timeValueMap = Map.fromList (zip times values)
        extendValues = map (\v -> Map.lookup v timeValueMap)
                          completeTimes

-- fileToTS to easily convert your file data into TS types
fileToTS :: [(Int,a)] -> TS a
fileToTS tvPairs = createTS times values
  where (times, values) = unzip tvPairs

-- showTVPair to render time/value pairs readable
showTVPair :: Show a => Int -> (Maybe a) -> String
showTVPair time (Just value) = mconcat [show time, "|", show value, "\n"]
showTVPair time Nothing = mconcat [show time, "|NA\n"]




-- Making TS an instance of Show by using zipWith and showTVPair
instance Show a => Show (TS a) where
  show (TS times values) = mconcat rows
    where rows = zipWith showTVPair times values

-- Converting all your data files into TS types
ts1 :: TS Double
ts1 = fileToTS file1

ts2 :: TS Double
ts2 = fileToTS file2

ts3 :: TS Double
ts3 = fileToTS file3

ts4 :: TS Double
ts4 = fileToTS file4

--20.2. STITCHING TOGETHER TS DATA WITH SEMIGROUP AND MONOIDから
