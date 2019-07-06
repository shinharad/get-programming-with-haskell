{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Data.Text      as T
import qualified Data.Text.IO   as TIO

import           Data.Semigroup

firstWord :: String
firstWord = "pesimism"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

myWord :: T.Text
myWord = "dog"

--
-- lines
--
sampleInput :: T.Text
sampleInput = "this\nis\ninput"

-- GHCi>T.lines sampleInput
-- ["this","is","input"]
--
-- words
--
someText :: T.Text
someText = "Some\ntext for\t you"

-- GHCi> T.words someText
-- ["Some","text","for","you"]
--
-- splitOn
--
breakText :: T.Text
breakText = "simple"

exampleText :: T.Text
exampleText = "This is simple to do"

-- GHCi> T.splitOn breakText exampleText
-- ["This is "," to do"]
--
-- unlines
--
-- GHCi> sampleInput
-- "this\nis\ninput"
-- GHCi> T.lines sampleInput
-- ["this","is","input"]
-- GHCi> T.unlines (T.lines sampleInput)
-- "this\nis\ninput\n"
--
-- unwords
--
-- GHCi> someText
-- "Some\ntext for\t you"
-- GHCi> T.words someText
-- ["Some","text","for","you"]
-- GHCi> T.unwords (T.words someText)
-- "Some text for you"
--
-- intercalate
--
-- GHCi> exampleText
-- "This is simple to do"
-- GHCi> T.splitOn breakText exampleText
-- ["This is "," to do"]
-- GHCi> T.intercalate breakText (T.splitOn breakText exampleText)
-- "This is simple to do"
combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some", " ", "text"]

combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " " <> "text"

dharma :: T.Text
dharma = "धर्म"

bgText :: T.Text
bgText = "श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्। स्वधर्मे निधनं श्रेयः परधर्मो भयावहः"

highlight :: T.Text -> T.Text -> T.Text
highlight query fullText = T.intercalate highlighted pieces
  where
    pieces = T.splitOn query fullText
    highlighted = mconcat ["{", query, "}"]
