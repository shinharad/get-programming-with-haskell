{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import System.Random
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

sampleBytes :: B.ByteString
sampleBytes = "Hello!"

-- これはコンパイルエラー
-- ByteStringのunpackは、[GHC.Word.Word8]を返す
--sampleString :: String
--sampleString = B.unpack sampleBytes

bcInt :: BC.ByteString
bcInt = "6"

-- ByteString.Char8のunpackは、[Char]を返す
bcToInt :: BC.ByteString -> Int
bcToInt = read . BC.unpack


main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  imageFile <- BC.readFile fileName
  glitched <- randomReplaceByte imageFile
--  glitched <- return imageFile -- 評価させたうえでIOで返す
  let glitchedFileName = mconcat ["glitched_", fileName]
  BC.writeFile glitchedFileName glitched
  print "all done"


-- intToChar creates a valid byte from an Int
intToChar :: Int -> Char
intToChar int = toEnum safeInt
  where safeInt = int `mod` 255

-- intToBC takes an Int and gives you a single-character ByteString
intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

-- replaceByte removes a byte and replaces it with a new one
replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc charVal bytes = mconcat [before, newChar, after]
  where (before,rest) = BC.splitAt loc bytes
        after = BC.drop 1 rest
        newChar = intToBC charVal

-- randomReplaceByte applies random numbers to replaceByte
randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
  let bytesLength = BC.length bytes
  location <- randomRIO (1, bytesLength)
  charVal <- randomRIO (0, 255)
  return (replaceByte location charVal bytes)


-- 25.2.2. Sorting random bytesから