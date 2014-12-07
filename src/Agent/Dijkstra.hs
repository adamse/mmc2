{-# LANGUAGE RecordWildCards #-}
-- |
-- An agent that implements Dijksta's
-- algorithm to find shortest path
module Agent.Dijkstra (
  DijkstraAgent()
  ) where

import Control.Monad.State.Strict
import Data.Foldable as F
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M

import Agent
import FromServer
import ToServer

type Target = Position

-- | Constructor of Agent
data DijkstraAgent = DA
  { currentPath :: [Move]
  , currentTarget :: Maybe Position
  } deriving (Show, Eq)

type DistanceMap = M.Map Position Int
type PrevMap = M.Map Position Position
type PositionedLayout = M.Map Position Tile

instance Agent DijkstraAgent where
  newAgent = return (DA [] Nothing)
  killAgent _ = return ()
  stepAgent fs@(FromServer {..}) =
    if ((Carryable Banana) `Prelude.elem` inventory)
        then return (Use Banana)
        else
      if (inventoryUsage fs)  < inventorySize
            then goValuable fs
            else goUser fs

-- | Gets the size of the inventory and ignores bananas, since they are used instantaneously anyway
inventoryUsage :: FromServer -> Int
inventoryUsage fs@(FromServer {..})
  | Carryable Banana `Prelude.elem` inventory = invLength - 1
  | otherwise = invLength
  where invLength = length inventory

goValuable :: FromServer -> StateT DijkstraAgent IO Command
goValuable (FromServer {..}) = do
    liftIO $ print "Valuable"
    let (dm, pm) = dijkstra position pl (neighbours movv pl)
    let target = getTarget dm pl (goodness inventoryRatio)
    let path = reverse $ constructPath pm position target
    return (if length path < 2
            then Idle
            else getCommand path (fromMaybe False (fmap speedyp buffs)))
  where
    pl = positionedLayout layout
    movv t = valuable t || carryable t || movable t || monkey t || user t
    inventoryRatio = fromIntegral (length inventory) / fromIntegral inventorySize

goUser (FromServer {..}) = do
    liftIO $ print "User"
    let (dm, pm) = dijkstra position pl (neighbours movv pl)
    let target = getTarget dm pl userGoodness
    let path = reverse $ constructPath pm position target
    return (if length path < 2
               then Idle
               else getCommand path (fromMaybe False (fmap speedyp buffs)))
  where
    pl = positionedLayout layout
    movv t = user t || movable t || monkey t

speedyp :: Buffs -> Bool
speedyp (Buffs speedy _) = case speedy of
                             Nothing -> False
                             Just n -> n > 0

getCommand path speedy = if speedy
                            then (Moves (take 2 ms))
                            else (Move (head ms))
  where ms = moves path

moves path = zipWith move path (tail path)

getTarget dm pl gn =
  M.foldrWithKey
    (\p _ currentBest ->
      case cmpGoodness pl dm gn p currentBest of
        GT -> currentBest
        _  -> p)
    (0,0)
    dm

cmpGoodness pl dm gn p1 p2 = compare (gn dm pl p1) (gn dm pl p2)

-- | Calculates the weighted distance to a node from the start node of the distance map
--   If the tile is a user the weighted distance depends on the ratio of the inventory content 
goodness :: Double -> DistanceMap -> PositionedLayout -> Position -> Int
goodness r dm pl p
  | valuable t || carryable t || user t || monkey t = d - prio
  | otherwise = 100000
  where d = fromJust $ M.lookup p dm
        t = fromJust $ M.lookup p pl
        prio = getGoodness t r

-- | Prioritizes the different tiles.
--   If the tile is a user it will prioritize it different depending on how much inventory we carry
getGoodness :: Tile -> Double -> Int
getGoodness (Valuable Playlist) _ = 8
getGoodness (Valuable Album) _ = 6
getGoodness (Valuable Song) _ = 4
getGoodness (Carryable Banana) _ = 3
getGoodness User r
  | r > 0.75 = 6
  | r > 0.5 = 3
  | r > 0 = 2
  | otherwise = -100000              
getGoodness _ _ = -1000000


userGoodness :: DistanceMap -> PositionedLayout-> Position -> Int
userGoodness dm pl p = if t then dist else dist + 100000
  where t = maybe False user (M.lookup p pl)
        dist = fromMaybe 100000 (M.lookup p dm)

-- | Which move to choose to move from p1 to neighbour p2
move :: Position -> Position -> Move
move (y1, x1) (y2, x2)
  | x1 < x2 = R
  | x1 > x2 = L
  | y1 > y2 = U
  | otherwise = D

constructPath :: PrevMap -> Position -> Target -> [Position]
constructPath pm p t
  | p == t    = [p]
  | otherwise =
    let prev = fromJust (M.lookup t pm)
    in t : constructPath pm p prev

newDistance :: Position -> DistanceMap
newDistance us = M.singleton us 0

dijkstra :: Position -> PositionedLayout -> (Position -> Set Position) -> (DistanceMap, PrevMap)
dijkstra start layout graph = dijkstra' (newDistance start) M.empty (S.insert start $ allnodes layout) graph

dijkstra' :: DistanceMap -> PrevMap -> Set Position -> (Position -> Set Position) -> (DistanceMap,PrevMap)
dijkstra' dist prevs unvisited neigh
  | S.null unvisited = (dist, prevs)
  | otherwise = dijkstra' newDists newPrevs nodesLeft neigh
  where closestNode = F.minimumBy (cmpDist dist) unvisited
        nodesLeft   = S.delete closestNode unvisited
        neighbours  = neigh closestNode
        (newDists, newPrevs) = S.foldr (compareDistance closestNode) (dist,prevs) neighbours

compareDistance :: Position -> Position -> (DistanceMap, PrevMap) -> (DistanceMap, PrevMap)
compareDistance pos neighbour (d, p)
  | alt < dist = (M.insert neighbour alt d, M.insert neighbour pos p)
  | otherwise = (d, p)
  where alt = distance d pos + 1
        dist = distance d neighbour

distance :: DistanceMap -> Position -> Int
distance m p = fromMaybe 100000 (M.lookup p m)

cmpDist :: DistanceMap -> Position -> Position -> Ordering
cmpDist m p1 p2 = let d1 = distance m p1
                      d2 = distance m p2
                   in compare d1 d2

neighbours :: (Tile -> Bool) -> PositionedLayout -> Position -> Set Position
neighbours movv playout (x, y) =
  S.fromList
  . map fst
  . filter (maybe False movv . snd)
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
