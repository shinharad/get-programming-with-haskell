module Book where

import           Data.Aeson
import           Data.ByteString.Lazy       as B
import           Data.ByteString.Lazy.Char8 as BC
import           Data.Text                  as T
import           GHC.Generics

data Book =
  Book
    { title  :: T.Text
    , author :: T.Text
    , year   :: Int
    }
  deriving (Show, Generic)

instance FromJSON Book

instance ToJSON Book

myBook :: Book
myBook = Book {author = "Will Kurt", title = "Learn Haskell", year = 2017}

myBookJSON :: BC.ByteString
myBookJSON = encode myBook

wrongJSON :: BC.ByteString
wrongJSON = "{\"writer\":\"Emil Cioran\",\"title\":\"A Short History of Decay\",\"year\"=1949}"

bookFromWrongJSON :: Maybe Book
bookFromWrongJSON = decode wrongJSON

--
-- GHCi> eitherDecode collectJSON :: Either String Book
-- Right (Book {title = "Learn Haskell", author = "Will Kurt", year = 2017})
--
collectJSON :: BC.ByteString
collectJSON = "{\"year\":2017,\"author\":\"Will Kurt\",\"title\":\"Learn Haskell\"}"

