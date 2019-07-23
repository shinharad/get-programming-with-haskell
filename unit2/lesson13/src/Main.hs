module Main where

main :: IO ()
main = return ()

addThenDouble :: Num a => a -> a -> a
addThenDouble x y = (x + y) * 2

data Icecream = Chocolate | Vanilla deriving (Show, Eq, Ord)

inc :: Int -> Int
inc x = x + 1

inc' :: Num a => a -> a
inc' x = x + 1

main1 = do
  print $ Vanilla == Vanilla -- True
  print $ Chocolate == Vanilla -- False
  print $ Chocolate /= Vanilla -- True
