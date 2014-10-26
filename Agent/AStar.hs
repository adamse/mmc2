{-# LANGUAGE RecordWildCards #-}

-- |
-- An AStar agent implementation
module Agent.AStar (
  AStarAgent ()
  ) where

import Control.Monad.State.Strict
import Data.List
import Data.Function
import Data.Ord
import System.Random

import Agent
import GameTypes

type Target = Position

-- | The internal state of the AI agent
data AStarAgent = Void
  deriving (Show, Eq)

instance Agent AStarAgent where
  newAgent = return Void
  killAgent _ = return ()
  stepAgent (GameState {..}) = do
    let playout = positionedLayout layout
    let target = acquireTarget position playout
    let (m:_) = astar position target layout
    return m

-- | Perform A* search on grid
astar :: Position -> Position -> [[Tile]] -> [Move]
astar current target map = undefined

positionedLayout :: [[Tile]] -> [(Position, Tile)]
positionedLayout layout
  = Prelude.concat
  $ zipWith (\r x -> zipWith (\t y -> ((x, y), t)) r [0..]) layout [0..]

-- | Find closes desirable thing!
acquireTarget :: Position -> [(Position, Tile)] -> Target
acquireTarget pos layout
  = fst
  . minimumBy (compare `on` (dist pos . fst)) 
  . filter (isThing . snd)
  $ layout
  where isThing (Thing _) = True
        isThing _         = False

-- | Some measure of distance between positions
dist :: Position -> Position -> Int
dist (x1, y1) (x2, y2) = sqr (x1 - x2) + sqr (y1 - y2)
  where sqr x = x * x
