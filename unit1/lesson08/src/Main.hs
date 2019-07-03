module Main where

main :: IO ()
main = do
  putStrLn "hello world"

add3ToAll [] = []
add3ToAll (x:xs) = (3 + x):add3ToAll xs

concatAll xs = foldl (++) "" xs

sumOfSquares xs = foldl (+) 0 (map (^2) xs)

rcons x y = y:x
myReverse xs = foldl rcons [] xs
