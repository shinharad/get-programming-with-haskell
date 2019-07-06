{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Data.Text          as T
import qualified Data.Text.IO       as TI
import           System.Environment
import           System.IO

getCounts :: String -> (Int, Int, Int)
getCounts input = (charCount, wordCount, lineCount)
  where
    charCount = length input
    wordCount = (length . words) input
    lineCount = (length . lines) input

countsText :: (Int, Int, Int) -> String
countsText (cc, wc, lc) = unwords ["chars:", show cc, "words:", show wc, "lines:", show lc]

-- STRICT I/O
-- StringをData.Textに書き換えれば正格評価となるので、読み込みの評価を強制させることができる
getCounts' :: T.Text -> (Int, Int, Int)
getCounts' input = (charCount, wordCount, lineCount)
  where
    charCount = T.length input
    wordCount = (length . T.words) input
    lineCount = (length . T.lines) input

countsText' :: (Int, Int, Int) -> T.Text
countsText' (cc, wc, lc) = T.pack (unwords ["chars: ", show cc, " words: ", show wc, " lines: ", show lc])
