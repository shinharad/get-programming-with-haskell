{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TI

-- 入力ファイルより2行読み込んでファイルに書き込む
main' :: IO ()
main' = do
  helloFile <- openFile "hello.txt" ReadMode
  firstLine <- hGetLine helloFile
  putStrLn firstLine
  secondLine <- hGetLine helloFile
  goodbyeFile <- openFile "goodbye.txt" WriteMode
  hPutStrLn goodbyeFile secondLine
  hClose helloFile
  hClose goodbyeFile
  putStrLn "done!"

-- ファイルがEOFか判定してから1行取得する
main'' :: IO ()
main'' = do
  helloFile <- openFile "hello.txt" ReadMode
  hasLine <- hIsEOF helloFile
  firstLine <- if not hasLine
               then hGetLine helloFile
               else return "empty"
  putStrLn "done!"


-- SIMPLE I/O TOOLS
main''' :: IO()
main''' = do
  args <- getArgs
  let fileName = head args
  file <- openFile fileName ReadMode
  input <- hGetContents file
  let summary = (countsText . getCounts) input
  putStrLn summary -- ここでsummaryを使用することで、ファイル読み込みが評価される
  hClose file
  appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])

-- hGetContentsは遅延評価のため使うまで評価されない
-- inputは使用されるまで評価されないが、それはsummaryも同様で、
-- appendFileのところでようやく評価されるが、ここでhCloseが発動してしまうので読み込みに失敗してしまう
--main :: IO()
--main = do
--  args <- getArgs
--  let fileName = head args
--  file <- openFile fileName ReadMode
--  input <- hGetContents file
--  hClose file
--  let summary = (countsText . getCounts) input
--  appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])
--  putStrLn summary

getCounts :: String -> (Int, Int, Int)
getCounts input = (charCount, wordCount, lineCount)
  where charCount = length input
        wordCount = (length . words) input
        lineCount = (length . lines) input

countsText :: (Int, Int, Int) -> String
countsText (cc, wc, lc) = unwords [ "chars:"
                                    , show cc
                                    , "words:"
                                    , show wc
                                    , "lines:"
                                    , show lc ]

-- STRICT I/O

-- StringをData.Textに書き換えれば正格評価となるので、読み込みの評価を強制させることができる
getCounts' :: T.Text -> (Int, Int, Int)
getCounts' input = (charCount, wordCount, lineCount)
 where charCount = T.length input
       wordCount = (length . T.words) input
       lineCount = (length . T.lines) input

countsText' :: (Int,Int,Int) -> T.Text
countsText' (cc,wc,lc) = T.pack (unwords ["chars: "
                                   , show cc
                                   , " words: "
                                   , show wc
                                   , " lines: "
                                   ,  show lc])

main :: IO()
main = do
  args <- getArgs
  let fileName = head args
  input <- TI.readFile fileName
  let summary = (countsText' . getCounts') input
  TI.appendFile "stats.data" (mconcat [T.pack fileName, " ", summary, "\n"])
  TI.putStrLn summary

