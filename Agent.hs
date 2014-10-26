module Agent where

import Control.Monad.State.Strict

import GameTypes

-- | Interface for an agent
class Agent agentState where
  -- | Set up a new agent
  newAgent :: IO agentState 
  -- | Tear down agent. Useful to save final state to disk
  killAgent :: agentState  -> IO ()
  -- | Let the agent take a step
  stepAgent :: Monad m => GameState -> StateT agentState m Move
