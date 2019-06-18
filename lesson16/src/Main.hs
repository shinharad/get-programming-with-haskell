module Main where

main :: IO ()
main = do
  putStrLn "hello world"

type FirstName = String
type LastName = String
type MiddleName = String

data Name = Name FirstName LastName
   | NameWithMiddle FirstName MiddleName LastName
   | TwoInitialsWithLast Char Char LastName
   | FirstNameWithTwoInits FirstName Char Char

data Creator = AuthorCreator Author | ArtistCreator Artist

data Author = Author Name

data Artist = Person Name | Band String

hpLovecraft :: Creator
hpLovecraft = AuthorCreator
              (Author
                (TwoInitialsWithLast 'H' 'P' "Lavecraft"))

-- 16.3. PUTTING TOGETHER YOUR BOOKSTORE
data StoreItem = BookItem Book
 | RecordItem VinylRecord
 | ToyItem CollectibleToy

data Book = Book {
    author    :: Creator
  , isbn      :: String
  , bookTitle :: String
  , bookYear  :: Int
  , bookPrice :: Double
  }

data VinylRecord = VinilRecord {
    artist      :: Creator
  , recordTitle :: String
  , recordYear  :: Int
  , recordPrice :: Double
  }

data CollectibleToy = CollectibleToy {
    name :: String
  , description :: String
  , toyPrice :: Double
  }

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
