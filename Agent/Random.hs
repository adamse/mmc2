-- |
-- Implementation of dumb random AI
module Agent.Random (
    Agent(..)
  , AgentState
  ) where

import Control.Monad.State.Strict
import System.Random

import GameTypes
import Agent

data AgentState = AS { gen :: StdGen }

instance Agent AgentState where
  newAgent = fmap AS newStdGen
  killAgent _ = return ()
  stepAgent gs = StateT $ \(AS g) ->
    let (m, g') = random g
    in return (m, AS g')
