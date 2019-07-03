{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString    as B
import           Data.Maybe
import qualified Data.Text          as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO       as TIO

main :: IO ()
main = do
  marcData <- B.readFile "sample.mrc"
  let processed = processRecords 500 marcData
  TIO.writeFile "books.html" processed

type Author = T.Text

type Title = T.Text

data Book =
  Book
    { author :: Author
    , title  :: Title
    }
  deriving (Show)

type Html = T.Text

bookToHtml :: Book -> Html
bookToHtml book = mconcat ["<p>\n", titleInTags, authorInTags, "</p>\n"]
  where
    titleInTags = mconcat ["<strong>", title book, "</strong>\n"]
    authorInTags = mconcat ["<em>", author book, "</em>\n"]

booksToHtml :: [Book] -> Html
booksToHtml books =
  mconcat
    [ "<html>\n"
    , "<head><title>books</title>"
    , "<meta charset='utf-8'/>"
    , "</head>\n"
    , "<body>\n"
    , booksHtml
    , "\n</body>\n"
    , "</html>"
    ]
  where
    booksHtml = (mconcat . map bookToHtml) books

type MarcRecordRaw = B.ByteString

type MarcLeaderRaw = B.ByteString

-- Declaring the length of the leader to be 24
leaderLength :: Int
leaderLength = 24

-- getLeader grabs the first 24 bytes of the record
getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader = B.take leaderLength

--getLeader record = B.take leaderLength record
-- E.decodeUtf8 :: ByteString -> Text
-- T.unpack :: Text -> String
rawToInt :: B.ByteString -> Int
rawToInt = read . T.unpack . E.decodeUtf8

getRecordLength :: MarcLeaderRaw -> Int
getRecordLength leader = rawToInt (B.take 5 leader)

-- nextAndRest breaks a stream of records into a head and tail
nextAndRest :: B.ByteString -> (MarcRecordRaw, B.ByteString)
nextAndRest marcStream = B.splitAt recordLength marcStream
  where
    recordLength = getRecordLength marcStream

-- Converting a stream of raw data into a list of records
allRecords :: B.ByteString -> [MarcRecordRaw]
allRecords marcStream =
  if marcStream == B.empty
    then []
    else next : allRecords rest
  where
    (next, rest) = nextAndRest marcStream

type MarcDirectoryRaw = B.ByteString

-- Getting the base address to determine the size of the directory
getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress leader = rawToInt (B.take 5 remainder)
  where
    remainder = B.drop 12 leader

-- Calculating the length of the directory with getDirectoryLength
getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = getBaseAddress leader - (leaderLength + 1)

-- Putting everything together to getDirectory
getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record = B.take directoryLength afterLeader
  where
    directoryLength = getDirectoryLength record
    afterLeader = B.drop leaderLength record

-- MarcDirectoryRaw type synonym and dirEntryLength
type MarcDirectoryEntryRaw = B.ByteString

dirEntryLength :: Int
dirEntryLength = 12

-- splitDirectory breaks down the directory into its entries
splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory directory =
  if directory == B.empty
    then []
    else nextEntry : splitDirectory restEntries
  where
    (nextEntry, restEntries) = B.splitAt dirEntryLength directory

data FieldMetadata =
  FieldMetadata
    { tag         :: T.Text
    , fieldLength :: Int
    , fieldStart  :: Int
    }
  deriving (Show)

-- Converting a raw directory entry into a FieldMetadata type
makeFieldMetadata :: MarcDirectoryEntryRaw -> FieldMetadata
makeFieldMetadata entry = FieldMetadata textTag theLength theStart
  where
    (theTag, rest) = B.splitAt 3 entry
    textTag = E.decodeUtf8 theTag
    (rawLength, rawStart) = B.splitAt 4 rest
    theLength = rawToInt rawLength
    theStart = rawToInt rawStart

-- Mapping makeFieldMetadata to [FieldMetadata]
getFieldMetadata :: [MarcDirectoryEntryRaw] -> [FieldMetadata]
getFieldMetadata = map makeFieldMetadata

--getFieldMetadata rawEntries = map makeFieldMetadata rawEntries
-- Type synonym for FieldText
type FieldText = T.Text

-- Getting the FieldText
getTextField :: MarcRecordRaw -> FieldMetadata -> FieldText
getTextField record fieldMetadata = E.decodeUtf8 byteStringValue
  where
    recordLength = getRecordLength record
    baseAddress = getBaseAddress record
    baseRecord = B.drop baseAddress record
    baseAtEntry = B.drop (fieldStart fieldMetadata) baseRecord
    byteStringValue = B.take (fieldLength fieldMetadata) baseAtEntry

-- Getting the field delimiter
fieldDelimiter :: Char
fieldDelimiter = toEnum 31

-- Tags and subfield codes for title and author
titleTag :: T.Text
titleTag = "245"

titleSubfield :: Char
titleSubfield = 'a'

authorTag :: T.Text
authorTag = "100"

authorSubfield :: Char
authorSubfield = 'a'

-- Safely looking up FieldMetadata from the directory
lookupFieldMetadata :: T.Text -> MarcRecordRaw -> Maybe FieldMetadata
lookupFieldMetadata aTag record =
  if length results < 1
    then Nothing
    else Just (head results)
  where
    metadata = (getFieldMetadata . splitDirectory . getDirectory) record
    results = filter ((== aTag) . tag) metadata

-- Safely looking up a potentially missing subfield
lookupSubfield :: Maybe FieldMetadata -> Char -> MarcRecordRaw -> Maybe T.Text
lookupSubfield Nothing subfield record = Nothing
lookupSubfield (Just fieldMetadata) subfield record =
  if null results
    then Nothing
    else Just ((T.drop 1 . head) results)
  where
    rawField = getTextField record fieldMetadata
    subfields = T.split (== fieldDelimiter) rawField
    results = filter ((== subfield) . T.head) subfields

-- General lookupValue function for looking up tag-subfield code pairs
lookupValue :: T.Text -> Char -> MarcRecordRaw -> Maybe T.Text
lookupValue aTag subfield record = lookupSubfield entryMetadata subfield record
  where
    entryMetadata = lookupFieldMetadata aTag record

-- Specific cases of looking up Title and Author
lookupTitle :: MarcRecordRaw -> Maybe Title
lookupTitle = lookupValue titleTag titleSubfield

lookupAuthor :: MarcRecordRaw -> Maybe Author
lookupAuthor = lookupValue authorTag authorSubfield

-- Raw MARC records to Maybe Title, Maybe Author pairs
marcToPairs :: B.ByteString -> [(Maybe Title, Maybe Author)]
marcToPairs marcStream = zip titles authors
  where
    records = allRecords marcStream
    titles = map lookupTitle records
    authors = map lookupAuthor records

-- Convert Maybe values into Books
pairsToBooks :: [(Maybe Title, Maybe Author)] -> [Book]
pairsToBooks pairs = map (\(title, author) -> Book {title = fromJust title, author = fromJust author}) justPairs
  where
    justPairs = filter (\(title, author) -> isJust title && isJust author) pairs

-- Putting it all together in processRecords
processRecords :: Int -> B.ByteString -> Html
processRecords n = booksToHtml . pairsToBooks . take n . marcToPairs
