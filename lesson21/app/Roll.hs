module Roll where

import System.Random

main' :: IO ()
main' = do
  dieRoll <- randomRIO (minDie, maxDie)
  putStrLn (show dieRoll)

minDie :: Int
minDie = 1

maxDie :: Int
maxDie = 6

