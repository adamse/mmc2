-- |
-- Module implementing the AI.
--
-- The interface is a 'nextMove', a possible state container 'AIState' and a new state 'newAIState'.
module AI (
    nextMove
  , AIState()
  , newAIState
  ) where

import Control.Monad.State.Strict
import System.Random

import Types

-- | The internal state of the AI agent
type AIState = ()

-- | A fresh AIState
newAIState :: AIState
newAIState = ()

-- | Function to calculate the next move
nextMove :: GameState -> StateT AIState IO Move
nextMove _ = liftIO randomIO
