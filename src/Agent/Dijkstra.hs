-- An agent that implements Dijksta's 
--  algorithm to find shortest path
module Agent.Dijkstra (
	DijkstraAgent() 
	) where

import Control.Monad.State.Strict
import Data.List
import Data.Function
import Data.Graph.AStar
import Data.Maybe
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as S
import Data.PSQueue
import System.Random

import Agent
import GameTypes
import ToServer

type Target = Position

-- | Constructor of Agent
data DijkstraAgent = Void
	deriving (Show, Eq)

instance Agent DijkstraAgent where
	newAgent = return Void
	killAgent _ = return ()
	stepAgent (GameState {..}) = 
		return $ move position (path !! 1)
	  where
	  	(Just path) = undefined
