module Agent where

import Control.Monad.State.Strict

import Types

class Agent agentState where
  newAgent :: IO agentState 
  killAgent :: agentState  -> IO ()
  stepAgent :: Monad m => GameState -> StateT agentState m Move
