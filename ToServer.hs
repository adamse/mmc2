{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Implements all types and instances needed to communicate with the game server
module ToServer (
  -- * To server
    ApiKey
  , Move (..)
  , Command (..)
  ) where

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

data Response = Response ApiKey Command
  deriving (Eq, Show, Generic)

instance ToJSON Response

-- | A command
--
-- Encoding a command:
--
-- >>> let apiKey = "YOUR API KEY"
-- >>> let move = Move apiKey U
-- >>> encode move
-- "{\"apiKey\":\"YOUR API KEY\",\"command\":\"move\",\"move\":\"up\"}"
data Command
  = Idle
  | Move Move
  | Moves [Move]
  | JoinGame
  deriving (Eq, Show)

instance ToJSON Command where
  toJSON = object . toPairs

toPairs Idle = [ "command" .= String "idle" ]
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

type GameId = Int

type Team = String

data Request = Request
  { apiKey :: ApiKey
  , gameId :: GameId
  , team :: Team
  , command :: Command
  } deriving (Eq, Show)

instance ToJSON Request where
  toJSON (Request apiKey gameId team command) = object (
    [ "team" .= team
    , "apiKey" .= apiKey
    , "gameId" .= gameId
    ] <> toPairs command)
