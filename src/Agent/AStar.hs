{-# LANGUAGE RecordWildCards #-}

-- |
-- An AStar agent implementation
module Agent.AStar (
  AStarAgent ()
  ) where

import Control.Monad.State.Strict
import Data.List
import Data.Function
import Data.Graph.AStar
import Data.Maybe
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as S
import System.Random

import Agent
import FromServer
import ToServer

type Target = Position

-- | The internal state of the AI agent
data AStarAgent = Void
  deriving (Show, Eq)

instance Agent AStarAgent where
  newAgent = return Void
  killAgent _ = return ()
  stepAgent (FromServer {..}) =
    return $ Move $ move position (path !! 1)
   where
    (Just path) = aStar
      (neighbours playout)
      (\_ _ -> 1)
      (dist position)
      (goal playout)
      position
    playout = positionedLayout layout

-- | Which move to choose to move from p1 to neighbour p2
move :: Position -> Position -> Move
move (x1, y1) (x2, y2)
  | x1 < x2 = R
  | x1 > x2 = L
  | y1 < y2 = U
  | otherwise = D

-- | Do we want to go there?
goal :: [(Position, Tile)] -> Position -> Bool
goal playout p = maybe False valuable (lookup p playout)

neighbours :: [(Position, Tile)] -> Position -> Set Position
neighbours playout (x, y) =
  S.fromList
  . map fst
  . filter (maybe False movable . snd)
  . zip possible
  . map (`lookup` playout)
  $ possible
 where
  possible =
    [ (x-1, y), (x+1, y)
    , (x, y-1), (x, y+1)
    ]

isThing (Valuable _) = True
isThing _         = False

positionedLayout :: [[Tile]] -> [(Position, Tile)]
positionedLayout layout
  = Prelude.concat
  $ zipWith (\r x -> zipWith (\t y -> ((x, y), t)) r [0..]) layout [0..]

-- | Find closes desirable thing!
acquireTarget :: [(Position, Tile)] -> Position -> Target
acquireTarget layout pos
  = fst
  . minimumBy (compare `on` (dist pos . fst)) 
  . filter (isThing . snd)
  $ layout

-- | Some measure of distance between positions
dist :: Position -> Position -> Int
dist (x1, y1) (x2, y2) = floor . sqrt . fromIntegral $ sqr (x1 - x2) + sqr (y1 - y2)
  where sqr x = x * x
