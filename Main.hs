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
import System.Exit

import Agent
import Agent.Random
import GameTypes

-- | URL for API-endpoint.
apiEndPoint :: String
apiEndPoint = "http://warmup.monkeymusicchallenge.com/team/The%20Human%20League"

main :: IO ()
main = do
  args <- getArgs

  unless (not (null args)) $ do
    p <- getProgName
    putStrLn $ "Usage: " ++ p ++ " API_KEY"
    exitFailure

  let apiKey = head args

  -- Start a new game
  startReq <- liftM (setBody (encode (NewGame apiKey))) getBaseReq

  withManager $ \manager -> do
    response <- httpLbs startReq manager
    let gameState = getState response

    -- Set up new agent
    agentState <- liftIO (newAgent :: IO RandomAgent)
    -- Run agent
    finalState <- liftIO $ loop apiKey manager (fromJust gameState) agentState
    -- Tear down agent
    liftIO (killAgent finalState)

  return ()

-- | Base request to API with correct headers and method.
getBaseReq :: IO Request
getBaseReq = do
  req <- parseUrl apiEndPoint
  let rqh = (hContentType, "application/json") : requestHeaders req
  let req' = req { requestHeaders = rqh, method = "POST" }
  return req'

-- | Set the body of a request.
setBody :: BL.ByteString -> Request -> Request
setBody body req = req { requestBody = RequestBodyLBS body }

-- | Create a 'GameState' from JSON response.
getState :: Response BL.ByteString -> Maybe GameState
getState res = decode $ responseBody res

-- | Main agent loop.
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
