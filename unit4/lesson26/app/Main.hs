{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Book
import           BooksToHtml
import qualified Data.ByteString as B
import qualified Data.Text.IO    as TIO

main :: IO ()
main = do
  marcData <- B.readFile "sample.mrc"
  let processed = processRecords 500 marcData
  TIO.writeFile "books.html" processed

main' :: IO ()
main' = TIO.writeFile "books.html" (booksToHtml myBooks)
