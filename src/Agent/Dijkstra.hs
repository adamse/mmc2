{-# LANGUAGE RecordWildCards #-}
-- |
-- An agent that implements Dijksta's 
-- algorithm to find shortest path
module Agent.Dijkstra (
	DijkstraAgent() 
	) where

import Debug.Trace

import Control.Monad.State.Strict
import Data.List
import Data.Function
import Data.Foldable as F
import Data.Maybe
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.PSQueue
import System.Random

import Agent
import FromServer
import ToServer

type Target = Position

-- | Constructor of Agent
data DijkstraAgent = Void
	deriving (Show, Eq)

type DistanceMap = M.Map Position Int
type PrevMap = M.Map Position Position
type PositionedLayout = M.Map Position Tile

instance Agent DijkstraAgent where
	newAgent = return Void
	killAgent _ = return ()
	stepAgent = dostuff

dostuff fs@(FromServer {..}) = do
    traceShowM fs
    traceShowM pl
    let (dm, pm) = dijkstra position pl (neighbours pl)
    traceShowM dm
    traceShowM pm
    let target = M.foldrWithKey (\p _ currentBest -> case cmpGoodness pl dm p currentBest of {
        GT -> currentBest; _  -> p }) (0,0) dm
    traceShowM target
    let path = reverse $ constructPath pm position target
    traceShowM path
    let m = move position (path !! 1)
    return (Move m)
  where
    pl = positionedLayout layout

-- | Which move to choose to move from p1 to neighbour p2
move :: Position -> Position -> Move
move (x1, y1) (x2, y2)
  | x1 < x2 = R
  | x1 > x2 = L
  | y1 < y2 = U
  | otherwise = D

constructPath :: PrevMap -> Position -> Target -> [Position]
constructPath pm p t
  | p == t    = [p]
  | otherwise = 
    let prev = fromJust (M.lookup t pm)
    in t : constructPath pm p prev

newDistance :: Position -> DistanceMap
newDistance us = M.singleton us 0

dijkstra :: Position -- ^ Start position
         -> PositionedLayout
         -> (Position -> Set Position) -- ^ The graph
         -> (DistanceMap, PrevMap)
dijkstra start layout graph = go start (newDistance start) M.empty (allnodes layout)
  where
    go current dist prev q =
     if S.null q
     then (dist, prev)
     else let u = F.minimumBy (cmpDist dist) q
              q' = S.delete u q
              (d, p) = S.foldr (\v (d, p) -> if distance d u + 1 < distance d v
                then (M.insert v (distance d u + 1) d, M.insert v u p)
                else (d, p)
               ) (dist, prev) (trace ("current graph" ++ show (graph u)) (graph u))

          in (d, p)

    distance :: DistanceMap -> Position -> Int
    distance m p = fromMaybe 100000 (M.lookup p m)
    cmpDist :: DistanceMap -> Position -> Position -> Ordering
    cmpDist m p1 p2 = let d1 = distance m p1
                          d2 = distance m p2
                       in compare d1 d2

cmpGoodness :: PositionedLayout -> DistanceMap -> Position -> Position -> Ordering
cmpGoodness pl dm p1 p2 = compare (goodness dm pl p1) (goodness dm pl p2)

goodness :: DistanceMap -> PositionedLayout -> Position -> Int
goodness dm pl p =
  let d = fromJust $ M.lookup p dm
      t = fromJust $ M.lookup p pl
      g = 4
  in if valuable t
     then d - g
     else 100000

neighbours :: PositionedLayout -> Position -> Set Position
neighbours playout (x, y) =
  S.fromList
  . map fst
  . filter (maybe False movable . snd)
  . zip possible
  . map (`M.lookup` playout)
  $ possible
 where
  possible =
    [ (x-1, y), (x+1, y)
    , (x, y-1), (x, y+1)
    ]

positionedLayout :: [[Tile]] -> PositionedLayout
positionedLayout layout
  = M.fromList
  $ Prelude.concat
  $ zipWith (\r x -> zipWith (\t y -> ((x, y), t)) r [0..]) layout [0..]

-- | All positions we can move to.
allnodes :: M.Map Position Tile -> Set Position
allnodes playout
  = S.fromList
  $ M.foldrWithKey (\k _ acc -> k:acc ) []
  $ M.filter movable playout
