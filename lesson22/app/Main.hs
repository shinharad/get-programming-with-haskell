module Main where

import System.Environment
import Control.Monad

-- mapM は値を返すが、mapM_ は値を捨てる
-- mapM :: Monad m => (a -> m b) -> t a -> m (t b)
-- mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()

-- replicateM は結果を返すが、
-- replicateM        :: (Applicative m) => Int -> m a -> m [a]
-- replicateM_ は結果を捨てる
-- replicateM_       :: (Applicative m) => Int -> m a -> m ()

--main :: IO ()
--main = do
--  args <- getArgs
--  let linesToRead = if length args > 0
--                    then read (head args)
--                    else 0 :: Int
--  numbers <- replicateM linesToRead getLine
--  let ints = map read numbers :: [Int]
--  print (sum ints)

--main :: IO ()
--main = do
--  args <- getArgs
----  mapM putStrLn args
--  mapM_ putStrLn args



-- 22.2. INTERACTING WITH LAZY I/O

-- stack runだと期待する動きになるが、
-- GHCiで実行すると入力後即座に表示される
main :: IO ()
main = do
  userInput <- getContents
  let numbers = toInts userInput
  print (sum numbers)

toInts :: String -> [Int]
toInts = map read . lines

