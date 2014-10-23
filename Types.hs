{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Implements all types and instances needed to communicate with the game server
--
-- Encoding a command:
--
-- >>> let apiKey = "YOUR API KEY"
-- >>> let move = Move apiKey U
-- >>> encode move
-- "{\"apiKey\":\"YOUR API KEY\",\"command\":\"move\",\"move\":\"up\"}"
module Types where

import Control.Applicative
import Control.Monad
import Data.Aeson
import GHC.Generics
import System.Random

type Position = (Int, Int)

-- | State of the game, as recieved from server.
data GameState = GameState
  { layout :: [[Tile]] -- ^ Current map layout
  , position :: Position -- ^ Our position in layout, indexed from (0, 0) at top left
  , turns :: Int -- ^ Turn left until end of game
  , pickedUp :: [Item] -- ^ Items currently carrying
  } deriving (Eq, Show, Generic)

-- Use fancy Generics to auto-derive this instance!! :D
instance FromJSON GameState

-- | Possible tiles in map
data Tile
  = Wall -- ^ A wall
  | Empty -- ^ Empty tile
  | User -- ^ User to deliver stuff to
  | Monkey -- ^ AI controlled monkey
  | Thing Item -- ^ A desirable thing
  deriving (Eq, Show)

instance FromJSON Tile where
  parseJSON (String t) = case t of
    "wall"     -> pure Wall
    "empty"    -> pure Empty
    "user"     -> pure User
    "monkey"   -> pure Monkey
    "song"     -> pure (Thing Song)
    "album"    -> pure (Thing Album)
    "playlist" -> pure (Thing Song)
    _          -> mzero -- Fail if not one of above types
  parseJSON _ = mzero -- Fail if not string

-- | Possible items on map
data Item
  = Song
  | Album
  | Playlist
  deriving (Eq, Show)

instance FromJSON Item where
  parseJSON (String i) = case i of
    "song"     -> pure Song
    "album"    -> pure Album
    "playlist" -> pure Playlist
    _          -> mzero
  parseJSON _ = mzero

-- | Possible moves
data Move
  = U | D | L | R
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


-- | Command to send to server
data Command
  = NewGame ApiKey
  | Move ApiKey Move
  deriving (Eq, Show)

-- | Type of api key
type ApiKey = String

instance ToJSON Command where
  toJSON (NewGame key) = 
    object [ "command" .= String "new game"
           , "apiKey"  .= key
           ]
  toJSON (Move key move) =
    object [ "command"   .= String "move"
           , "direction" .= move
           , "apiKey"    .= key
           ]
