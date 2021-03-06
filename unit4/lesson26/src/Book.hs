{-# LANGUAGE OverloadedStrings #-}

module Book where

import qualified Data.Text as T

type Author = T.Text

type Title = T.Text

data Book =
  Book
    { author :: Author
    , title  :: Title
    }
  deriving (Show)

book1 :: Book
book1 = Book {title = "The Conspiracy Against the Human Race", author = "Ligotti, Thomas"}

book2 :: Book
book2 = Book {title = "A Short History of Decay", author = "Cioran, Emil"}

book3 :: Book
book3 = Book {title = "The Tears of Eros", author = "Bataille, Georges"}

myBooks :: [Book]
myBooks = [book1, book2, book3]
