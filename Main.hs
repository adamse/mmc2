{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import System.Random

import Types

aPIKEYFILE = "apikey"
tEAMURL = "http://warmup.monkeymusicchallenge.com/team/The%20Human%20League"

main = do
  apiKey <- readFile aPIKEYFILE
  baseReq <- getBaseReq

  -- Start a new game
  let startReq = setBody baseReq (encode (NewGame apiKey))

  putStrLn "DEATH"

nextMove :: IO Move
nextMove = randomIO

getBaseReq :: IO Request
getBaseReq = do
  req <- parseUrl tEAMURL
  let rqh = (hContentType, "application/json") : requestHeaders req
  let req' = req { requestHeaders = rqh }
  return req'

setBody :: Request -> BL.ByteString -> Request
setBody req body = req { requestBody = RequestBodyLBS body }
