{-# LANGUAGE RecordWildCards #-}
-- |
-- An agent that implements Dijksta's 
-- algorithm to find shortest path
module Agent.Dijkstra {-(
  DijkstraAgent() 
  )-} where

import Debug.Trace

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
        else goToTarget fs 

{-                    
goValuable :: FromServer -> StateT DijkstraAgent IO Command
goValuable (FromServer {..}) = do
    liftIO $ print "Valuable"
    let (dm, pm) = dijkstra position pl (neighbours movv pl)
    let target = getTarget dm pl goodness
    let path = reverse $ constructPath pm position target
    let m = move position (path !! 1)
    return (Move m)
  where
    pl = positionedLayout layout
    movv t = valuable t || carryable t || movable t

goUser (FromServer {..}) = do
    liftIO $ print "User"
    let (dm, pm) = dijkstra position pl (neighbours movv pl)
    let target = getTarget dm pl userGoodness
    let path = reverse $ constructPath pm position target
    let m = move position (path !! 1)
    return (Move m)
  where
    pl = positionedLayout layout
    movv t = user t || movable t
-}

goToTarget :: FromServer -> StateT DijkstraAgent IO Command
goToTarget fs@(FromServer {..}) = do
    if inventoryFull
      then  liftIO $ print "Users"
      else  liftIO $ print "Valuables and carryables"
    let (dm, pm) = dijkstra position pl $
                   neighbours (getFiltering inventoryFull) pl
    let target = getTarget dm pl (calcPrio inventoryRatio)
    let path = reverse $ constructPath pm position target
    let m1 = move position (path !! 1)
    if (speedyBuffs fs) > 7
      then do
          let m2 = move (path !! 1) (path !! 2)
          return (Moves [m1,m2])
      else
      return (Move m1)
  where
    pl = positionedLayout layout
    inventoryFull = not (length inventory < inventorySize)
    inventoryRatio = d / i
      where d = fromIntegral $ length inventory :: Double
            i = fromIntegral $ inventorySize :: Double

-- | Gets the filter rules for tiles 
getFiltering :: Bool -- ^ Should we go to user?
                -> Tile -- ^ Tile to filter
                -> Bool 
getFiltering True tile  = user tile || movable tile || carryable tile || valuable tile
getFiltering False tile = movable tile || carryable tile || valuable tile

-- | Checks whether we have a speedy buff
speedyBuffs :: FromServer -> Int
speedyBuffs fs | hasBuffs = fromMaybe 0 (speedy $ fromJust $ buffs fs)
               | otherwise = 0
  where hasBuffs = isJust (buffs fs)

getTarget dm pl gn =
  M.foldrWithKey
    (\p _ currentBest ->
      case cmpGoodness pl dm gn p currentBest of 
        GT -> currentBest
        _  -> p)
    (0,0)
    dm

-- | Compares the priorities of two positions, p1 and p2, by calling a priority function with both positions as arguments. Returns the ordering of this comparison
cmpGoodness :: PositionedLayout -- ^ Map of positions and their tiles
               -> DistanceMap -- ^ Map of positions and their distances from a start node
               -> (DistanceMap -> PositionedLayout -> Position -> Int) -- ^ Priority function
               -> Position -- ^ First position
               -> Position -- ^ Second position
               -> Ordering -- ^ The result of comparing p1's and p2's priorities
cmpGoodness pl dm gn p1 p2 = compare (gn dm pl p1) (gn dm pl p2)

selectGoodness :: Bool -> (DistanceMap -> PositionedLayout -> Position -> Int)
selectGoodness inventoryFull | inventoryFull = userGoodness 
                             | otherwise = goodness

goodness :: DistanceMap -> PositionedLayout -> Position -> Int
goodness dm pl p =
  let d = fromJust $ M.lookup p dm
      t = fromJust $ M.lookup p pl
      prio = case t of
        Valuable Playlist -> 10
        Valuable Album -> 4
        Valuable Song -> 1
        Carryable Banana -> 4
        _ -> -100000
  in if (valuable t) || (carryable t)
     then d - prio
     else 100000

-- | Calculates the cost for a tile by subtracting the priority 
--   for that tile from the distance calculated in dijkstra
calcPrio :: Double -- ^ Length of inventory / inventorySize
            -> DistanceMap -- ^ Map of positions and their distances
            -> PositionedLayout -- ^ Map of positions and their tiles
            -> Position -- ^ Position to calculate
            -> Int -- ^ The weighted cost of a position
calcPrio r dm pl p = d - prio
  where d = trace (show p ++ show (distance dm p)) $ fromJust $ M.lookup p dm
        t = fromJust $ M.lookup p pl
        prio = getPrio t r 

-- | Gets the priorities for valuable tiles.
-- | Higher value means higher priority
getPrio :: Tile -- ^ The tile to get a value from
           -> Double -- ^ Ratio of inventory length and inventorysize
           -> Int -- ^ The prio of the tile
getPrio (Valuable Playlist) _ = 10
getPrio (Valuable Album) _ = 6
getPrio (Valuable Song) _ = 1
getPrio (Carryable Banana) _ = 8
getPrio User ratio | ratio > 0.75 = 15
                   | ratio > 0.5 = 3
                   | otherwise = (-100000)
getPrio _ _ = -10000000000

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

{-dijkstraOld :: Position -- ^ Start position
         -> PositionedLayout -- ^ Positioned layout
         -> (Position -> Set Position) -- ^ Neighbour function
         -> (DistanceMap, PrevMap)
dijkstraOld start layout graph = go (newDistance start) M.empty (S.insert start $ allnodes layout)
  where
    go d p q =
     if S.null q
     then (d, p)
     else let u = F.minimumBy (cmpDist d) q
              q' = S.delete u q
              (d', p') = S.foldr (\v (d, p) -> 
                let alt = distance d u + 1
                in if alt < distance d v
                   then (M.insert v alt d, M.insert v u p)
                   else (d, p)
               ) (d, p) $ graph u

          in go d' p' q'
-}

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

----------- Test data  ------------

fsTest = FromServer {layout = [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Valuable Playlist,Valuable Playlist,Valuable Playlist],[Empty,Carryable Banana,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall],[Empty,Empty,Valuable Song,Valuable Song,Empty,Empty,Empty,User,Wall,Valuable Playlist],[Valuable Album,Empty,Empty,Empty,Empty,Wall,Wall,Wall,Valuable Playlist,Valuable Playlist],[Wall,Wall,Carryable Banana,Valuable Album,Empty,Empty,Empty,Valuable Song,Wall,Empty],[Valuable Song,Carryable Banana,Wall,Wall,Wall,Valuable Album,Empty,Valuable Song,Wall,Empty],[Valuable Song,Wall,Valuable Album,Valuable Song,Valuable Song,Wall,Empty,Empty,Carryable Banana,Empty],[Monkey,Wall,Monkey,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,User,Empty,Empty,Wall,Wall,Empty,Empty,Wall,Wall],[Empty,Empty,Valuable Song,Valuable Song,Carryable Banana,Wall,Empty,Valuable Song,Valuable Song,Valuable Album]], position = (7,0), remainingTurns = 50, isGameOver = False, score = 0, buffs = Just (Buffs {speedy = Nothing, immobilized = Nothing}), inventory = [], inventorySize = 3}

plTest = positionedLayout (layout fsTest)
iRTest = (fromIntegral (length (inventory fsTest))) / (fromIntegral (inventorySize fsTest))
dmPmTest = dijkstra (position fsTest) plTest (neighbours (getFiltering True) plTest)
--dmPmTest2 = dijkstra (position fsTest) plTest (neighbours (getFiltering True) plTest)
