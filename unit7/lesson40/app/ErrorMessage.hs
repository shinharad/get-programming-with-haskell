module ErrorMessage where

import           Data.Aeson
import           Data.ByteString.Lazy       as B
import           Data.ByteString.Lazy.Char8 as BC
import           Data.Text                  as T
import           GHC.Generics

-- Writing your own instances of FromJSON and ToJSON
sampleError :: BC.ByteString
sampleError = "{\"message\":\"oops!\",\"error\": 123}"

-- "error"というプロパティが予約語とかぶる場合
data ErrorMessage =
  ErrorMessage
    { message :: T.Text
    , error   :: Int
    }
  deriving (Show)

--
-- This operator takes an Object (your JSON object) and some text and returns a value parsed into a context.
-- For example, this line of code is trying to parse the message field from your JSON object:
--   v .: "message"
--
-- GHCi> :t (.:)
-- (.:)
--   :: FromJSON a =>
--      Object -> Text -> aeson-1.4.2.0:Data.Aeson.Types.Internal.Parser a
--
instance FromJSON ErrorMessage where
  parseJSON (Object v) = ErrorMessage
   <$> v .: "message"
   <*> v .: "error"

instance ToJSON ErrorMessage where
  toJSON (ErrorMessage message errorCode) = object ["message" .= message, "error" .= errorCode]

--
-- GHCi> ErrorMessage <$> exampleMessage <*> exampleError
-- Just (ErrorMessage {message = "Opps", error = 123})
exampleMessage :: Maybe T.Text
exampleMessage = Just "Opps"

exampleError :: Maybe Int
exampleError = Just 123

sampleErrorMessage :: Maybe ErrorMessage
sampleErrorMessage = decode sampleError

--
-- GHCi> encode anErrorMessage
-- "{\"error\":0,\"message\":\"Everything is Okay\"}"
--
-- GHCi> eitherDecode "{\"error\":0,\"message\":\"Everything is Okay\"}" :: Either String ErrorMessage
-- Right (ErrorMessage {message = "Everything is Okay", error = 0})
--
anErrorMessage :: ErrorMessage
anErrorMessage = ErrorMessage "Everything is Okay" 0
