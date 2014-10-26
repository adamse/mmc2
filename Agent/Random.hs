-- |
-- Implementation of dumb random AI
module Agent.Random (
    Agent(..)
  , AgentState
  ) where

import Control.Monad.State.Strict
import System.Random

import Types
import Agent

data AgentState = AS { gen :: StdGen }

instance Agent AgentState where
  newAgent = do
    gen <- newStdGen
    return (AS gen)
  killAgent _ = return ()
  stepAgent gs = do
    AS g <- get
    let (move, g') = random g
    put (AS g')
    return move


