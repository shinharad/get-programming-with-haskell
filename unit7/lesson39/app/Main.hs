{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as LC
import           Network.HTTP.Simple

import           Lib

main :: IO ()
main = do
  res <- httpLBS "http://news.ycombinator.com"
  print (getResponseStatusCode res)

main' :: IO ()
main' = do
  let request = buildRequest (BC.pack "") (BC.pack "") (BC.pack "") (BC.pack "") -- example
  response <- httpLBS request
  let status = getResponseStatusCode response
  if status == 200
    then do
      print "saving request to file"
      let jsonBody = getResponseBody response
      L.writeFile "data.json" jsonBody
    else print "request failed with error"

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequest token host method path =
  setRequestMethod method $
  setRequestHost host $
  setRequestHeader "token" [token] $
  setRequestPath path $
  setRequestSecure True $
  setRequestPort 443 defaultRequest

-- こう書くのと同じこと
buildRequest' :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequest' token host method path =
  let state1 = setRequestPort 443 defaultRequest
   in let state2 = setRequestSecure True state1
       in let state3 = setRequestPath path state2
           in let state4 = setRequestHeader "token" [token] state3
               in setRequestHost host state4
