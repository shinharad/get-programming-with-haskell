{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as E
import qualified Data.Text.IO          as TIO
import           System.Environment
import           System.Random

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
  glitched <-
    foldM
      (\bytes func -> func bytes)
      imageFile
      [randomReplaceByte, randomSortSection, randomReplaceByte, randomSortSection, randomReplaceByte]
  let glitchedFileName = mconcat ["glitched_", fileName]
  BC.writeFile glitchedFileName glitched
  print "all done"

-- intToChar creates a valid byte from an Int
intToChar :: Int -> Char
intToChar int = toEnum safeInt
  where
    safeInt = int `mod` 255

-- intToBC takes an Int and gives you a single-character ByteString
intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

-- replaceByte removes a byte and replaces it with a new one
replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc charVal bytes = mconcat [before, newChar, after]
  where
    (before, rest) = BC.splitAt loc bytes
    after = BC.drop 1 rest
    newChar = intToBC charVal

-- randomReplaceByte applies random numbers to replaceByte
randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
  let bytesLength = BC.length bytes
  location <- randomRIO (1, bytesLength)
  charVal <- randomRIO (0, 255)
  return (replaceByte location charVal bytes)

-- sortSection sorts a section of bytes in your file
sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before, changed, after]
  where
    (before, rest) = BC.splitAt start bytes
    (target, after) = BC.splitAt size rest
    changed = BC.reverse (BC.sort target)

-- Randomizing your sortSection by using an IO action
randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
  let sectionSize = 25
  let bytesLength = BC.length bytes
  start <- randomRIO (0, bytesLength - sectionSize)
  return (sortSection start sectionSize bytes)

sampleText :: T.Text
sampleText = "山田太郎"

sampleTextSafe :: B.ByteString
sampleTextSafe = E.encodeUtf8 sampleText
-- GHCi> TIO.putStrLn (E.decodeUtf8 sampleTextSafe)
-- 山田太郎
