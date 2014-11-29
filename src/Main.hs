{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
{-import Control.Monad.IO.Class-}
import Control.Monad.State.Strict
import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import System.Environment
import System.Exit

import Agent
import Agent.Dijkstra
import FromServer
import ToServer

-- | URL for API-endpoint.
apiEndPoint :: String
apiEndPoint = "http://competition.monkeymusicchallenge.com/game"

main :: IO ()
main = do
  [team, apiKey, gameId] <- getArgs

  let toServer = ToServer team apiKey gameId

  print (toServer JoinGame)
  -- Start a new game
  startReq <- getReq (toServer JoinGame)

  print JoinGame

  withManager $ \manager -> do
    response <- httpLbs startReq manager
    let gameState = getState response

    -- Set up new agent
    agentState <- liftIO (newAgent :: IO DijkstraAgent)
    -- Run agent
    finalState <- liftIO $ loop toServer apiKey manager (fromJust gameState) agentState
    -- Tear down agent
    liftIO (killAgent finalState)

  return ()

-- | Base request to API with correct headers and method.
getReq :: ToServer -> IO Request
getReq t = do
  req <- parseUrl apiEndPoint
  let rqh = (hContentType, "application/json") : requestHeaders req
  let req' = req {
      requestHeaders = rqh
    , method = "POST"
    , requestBody = RequestBodyLBS (encode t)
    }
  return req'

-- | Create a 'FromServer' from JSON response.
getState :: Response BL.ByteString -> Maybe FromServer
getState res = decode $ responseBody res

-- | Main agent loop.
loop :: Agent a => (Command -> ToServer) -> ApiKey -> Manager -> FromServer -> a -> IO a
loop toServer apiKey manager = go
 where
  go gameState agentState
    | remainingTurns gameState <= 0 = return agentState
    | otherwise = do
      -- Step our agent
      (command, agentState') <- runStateT (stepAgent gameState) agentState

      print command

      -- Send new action to server
      req <- getReq (toServer command)
      res <- httpLbs req manager

      -- Rinse and repeat
      let gameState' = fromJust (getState res)
      print $ inventorySize gameState'
      go gameState' agentState'
