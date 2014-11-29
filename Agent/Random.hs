-- |
-- Implementation of dumb random AI
module Agent.Random (
  RandomAgent
  ) where

import Control.Monad.State.Strict
import System.Random

import ToServer
import FromServer
import Agent

-- | Contains a generator for random moves
data RandomAgent = AS { gen :: StdGen }

instance Agent RandomAgent where
  newAgent = fmap AS newStdGen
  killAgent _ = return ()
  stepAgent gs = StateT $ \(AS g) ->
    let (m, g') = random g
    in return (m, AS g')
