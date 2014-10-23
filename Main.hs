{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import System.Environment

import AI (nextMove, AIState, newAIState)
import Types

tEAMURL = "http://warmup.monkeymusicchallenge.com/team/The%20Human%20League"

main = do
  (apiKey:_) <- getArgs

  -- Start a new game
  startReq <- liftM (setBody (encode (NewGame apiKey))) getBaseReq

  withManager $ \manager -> do
    res <- httpLbs startReq manager
    let state = getState res
    liftIO $ print state
    liftIO $ loop apiKey manager (fromJust state) newAIState

  return ()

getBaseReq :: IO Request
getBaseReq = do
  req <- parseUrl tEAMURL
  let rqh = (hContentType, "application/json") : requestHeaders req
  let req' = req { requestHeaders = rqh, method = "POST" }
  return req'

setBody :: BL.ByteString -> Request -> Request
setBody body req = req { requestBody = RequestBodyLBS body }

getState :: Response BL.ByteString -> Maybe GameState
getState res = decode $ responseBody res

loop :: ApiKey -> Manager -> GameState -> AIState -> IO ()
loop apiKey manager gameState aiState
  | turns gameState <= 0 = return ()
  | otherwise        = do
    (m, aiState') <- runStateT (nextMove gameState) aiState
    req <- liftM (setBody (encode (Move apiKey m))) getBaseReq
    res <- httpLbs req manager
    print $ getState res
    let gameState' = fromJust (getState res)
    loop apiKey manager gameState' aiState'
