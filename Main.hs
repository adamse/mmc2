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

import Agent.Random
import GameTypes

tEAMURL = "http://warmup.monkeymusicchallenge.com/team/The%20Human%20League"

main = do
  (apiKey:_) <- getArgs

  -- Start a new game
  startReq <- liftM (setBody (encode (NewGame apiKey))) getBaseReq

  withManager $ \manager -> do
    response <- httpLbs startReq manager
    let gameState = getState response

    -- Set up new agent
    agentState <- liftIO (newAgent :: IO AgentState)
    -- Run agent
    finalState <- liftIO $ loop apiKey manager (fromJust gameState) agentState
    -- Tear down agent
    liftIO (killAgent finalState)

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

loop :: Agent a => ApiKey -> Manager -> GameState -> a -> IO a
loop apiKey manager gameState agentState
  | turns gameState <= 0 = return agentState
  | otherwise        = do
    -- Step our agent
    (m, agentState') <- runStateT (stepAgent gameState) agentState

    print m

    -- Send new action to server
    req <- liftM (setBody (encode (Move apiKey m))) getBaseReq
    res <- httpLbs req manager

    -- Rinse and repeat
    let gameState' = fromJust (getState res)
    loop apiKey manager gameState' agentState'

