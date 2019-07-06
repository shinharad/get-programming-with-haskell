{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as LC
import           Network.HTTP.Simple
import           Control.Monad.IO.Class

import           Lib

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequest token host method path =
  setRequestMethod method
    $ setRequestHost host
    $ setRequestHeader "token" [token]
    $ setRequestPath path
    $ setRequestSecure True
    $ setRequestPort 443 defaultRequest

-- こう書くのと同じこと
buildRequest' :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequest' token host method path =
  let state1 = setRequestPort 443 defaultRequest
   in let state2 = setRequestSecure True state1
       in let state3 = setRequestPath path state2
           in let state4 = setRequestHeader "token" [token] state3
               in setRequestHost host state4

myToken = "" :: BC.ByteString
host = "www.ncei.noaa.gov" :: BC.ByteString
path = "access/services/data/v1?dataset=global-summary-of-the-year&dataTypes=DP01,DP05,DP10,DSND,DSNW,DT00,DT32,DX32,DX70,DX90,SNOW,PRCP&stations=ASN00084027&startDate=1952-01-01&endDate=1970-12-31&includeAttributes=true&format=json" :: BC.ByteString

request = buildRequest myToken host "GET" path :: Request

main :: IO ()
main = do
  response <- httpLBS request
  let status = getResponseStatusCode response
  if status == 200
    then do
      print "saving request to file"
      let jsonBody = getResponseBody response
      L.writeFile "data.json" jsonBody
    else print "request failed with error"


response :: MonadIO m => m (Response LC.ByteString)
response = httpLBS "http://news.ycombinator.com"

main' :: IO ()
main' = do
  response <- httpLBS "http://news.ycombinator.com"
  print $ getResponseHeader "Server" response