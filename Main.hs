{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import System.Random
import System.Environment

import Types

tEAMURL = "http://warmup.monkeymusicchallenge.com/team/The%20Human%20League"

main = do
  (apiKey:_) <- getArgs
  baseReq <- getBaseReq
  print baseReq

  -- Start a new game
  let startReq = setBody baseReq (encode (NewGame apiKey))

  state <- withManager $ \manager -> do
    res <- httpLbs startReq manager
    let state = decode $ responseBody res :: Maybe GameState
    return state
  
  print state

nextMove :: IO Move
nextMove = randomIO

getBaseReq :: IO Request
getBaseReq = do
  req <- parseUrl tEAMURL
  let rqh = (hContentType, "application/json") : requestHeaders req
  let req' = req { requestHeaders = rqh, method = "POST" }
  return req'

setBody :: Request -> BL.ByteString -> Request
setBody req body = req { requestBody = RequestBodyLBS body }
