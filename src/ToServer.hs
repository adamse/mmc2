{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Implements all types and instances needed to communicate with the game server
module ToServer where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Aeson
import GHC.Generics
import System.Random

-- | Agent moves.
data Move
  = U -- ^ Up
  | D -- ^ Down
  | L -- ^ Left
  | R -- ^ Right
  deriving (Eq, Show, Enum, Bounded)

instance Random Move where
  random g =
    let (i, g') = randomR (0, 3) g
    in (toEnum i, g')
  randomR (lo, hi) g =
    let (i, g') = randomR (fromEnum lo, fromEnum hi) g
    in (toEnum i, g')

instance ToJSON Move where
  toJSON U = String "up"
  toJSON D = String "down"
  toJSON L = String "left"
  toJSON R = String "right"

-- | A command
data Command
  = Idle
  | JoinGame
  | Move Move
  | Moves [Move]
  deriving (Eq, Show)

instance ToJSON Command where
  toJSON = object . toPairs

toPairs Idle = [ "command" .= String "idle" ]
toPairs JoinGame = [ "command" .= String "join game" ]
toPairs (Move move) =
  [ "command" .= String "move"
  , "direction" .= move
  ]
toPairs (Moves moves) =
  [ "command" .= String "move"
  , "directions" .= moves
  ]

-- | API keys
type ApiKey = String

type GameId = String

type Team = String

data ToServer = ToServer
  { team :: Team
  , apiKey :: ApiKey
  , gameId :: GameId
  , command :: Command
  } deriving (Eq, Show)

instance ToJSON ToServer where
  toJSON (ToServer {..}) = object (
    [ "team" .= team
    , "apiKey" .= apiKey
    , "gameId" .= gameId
    ] <> toPairs command)
