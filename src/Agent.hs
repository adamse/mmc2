module Agent (Agent (..)) where

import Control.Monad.State.Strict

import FromServer
import ToServer

-- | Interface for an agent.
class Agent agentState where
  -- | Set up a new agent.
  newAgent :: IO agentState 
  -- | Tear down agent.
  killAgent :: agentState  -> IO ()
  -- | Let the agent take a step.
  stepAgent :: FromServer -> StateT agentState IO Command
