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
    if length inventory < inventorySize
      then goValuable fs
      else goUser fs

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
    movv t = valuable t || movable t

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

getTarget dm pl gn =
  M.foldrWithKey
    (\p _ currentBest ->
      case cmpGoodness pl dm gn p currentBest of 
        GT -> currentBest
        _  -> p)
    (0,0)
    dm

cmpGoodness pl dm gn p1 p2 = compare (gn dm pl p1) (gn dm pl p2)

goodness :: DistanceMap -> PositionedLayout -> Position -> Int
goodness dm pl p =
  let d = fromJust $ M.lookup p dm
      t = fromJust $ M.lookup p pl
      g = 4
  in if valuable t
     then d - g
     else 100000

userGoodness :: DistanceMap -> PositionedLayout-> Position -> Int
userGoodness _ pl p = if t then 0 else 100000
  where t = maybe False user (M.lookup p pl)

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

dijkstra :: Position -- ^ Start position
         -> PositionedLayout
         -> (Position -> Set Position) -- ^ The graph
         -> (DistanceMap, PrevMap)
dijkstra start layout graph = go (newDistance start) M.empty (S.insert start $ allnodes layout)
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
