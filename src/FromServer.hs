{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module FromServer where

import Control.Applicative
import Control.Monad
import Data.Aeson
import GHC.Generics

-- | Position on grid.
type Position = (Int, Int)

-- | State of the game, as recieved from server.
data FromServer = FromServer
  { layout :: [[Tile]] -- ^ Current map layout
  , position :: Position -- ^ Our position in layout, indexed from (0, 0) at top left
  , remainingTurns :: Int -- ^ Turn left until end of game
  , isGameOver :: Bool
  , score :: Int
  , buffs :: Maybe Buffs
  , inventory :: [Tile] -- ^ Items currently carrying
  , inventorySize :: Int
  } deriving (Eq, Show, Generic)

-- Use fancy Generics to auto-derive this instance!! :D
instance FromJSON FromServer

data Buffs = Buffs
  { speedy :: Maybe Int -- ^ speedy buff for some turns
  , immobilized :: Maybe Int -- ^ Stunned for some turns
  } deriving (Eq, Show, Generic)

instance FromJSON Buffs

-- | Tile types on map.
data Tile
  = Wall -- ^ A wall
  | User -- ^ User to deliver stuff to
  | Monkey -- ^ AI controlled monkey
  | Tunnel Int -- ^ A tunnel
  | Empty -- ^ Empty tile
  | ClosedDoor
  | OpenDoor
  | Lever
  | ArmedTrap
  | Valuable Valuable -- ^ A desirable thing
  | Carryable Carryable
  deriving (Eq, Show)

instance FromJSON Tile where
  parseJSON (String t) = case t of
    "wall"     -> pure Wall
    "user"     -> pure User
    "closed-door" -> pure ClosedDoor
    "armed-trap"  -> pure ArmedTrap
    "monkey"   -> pure Monkey

    "tunnel-1" -> pure (Tunnel 1)
    "tunnel-2" -> pure (Tunnel 2)
    "tunnel-3" -> pure (Tunnel 3)
    "tunnel-4" -> pure (Tunnel 4)
    "tunnel-5" -> pure (Tunnel 5)
    "tunnel-6" -> pure (Tunnel 6)
    "tunnel-7" -> pure (Tunnel 7)
    "tunnel-8" -> pure (Tunnel 8)
    "tunnel-9" -> pure (Tunnel 9)

    "empty"    -> pure Empty
    "open-door" -> pure OpenDoor

    "banana" -> pure (Carryable Banana)
    "trap" -> pure (Carryable Trap)

    "song"     -> pure (Valuable Song)
    "album"    -> pure (Valuable Album)
    "playlist" -> pure (Valuable Playlist)
    _          -> mzero -- Fail if not one of above types
  parseJSON _ = mzero -- Fail if not string

movable :: Tile -> Bool
movable t = t `elem` [Empty, OpenDoor]

carryable :: Tile -> Bool
carryable (Carryable _) = True
carryable _ = False

valuable :: Tile -> Bool
valuable (Valuable _) = True
valuable _ = False

tunnel :: Tile -> Bool
tunnel (Tunnel _) = True
tunnel _ = False

user :: Tile -> Bool
user User = True
user _ = False

-- | Item types.
data Valuable
  = Song
  | Album
  | Playlist
  deriving (Eq, Show)

instance FromJSON Valuable where
  parseJSON (String i) = case i of
    "song"     -> pure Song
    "album"    -> pure Album
    "playlist" -> pure Playlist
    _          -> mzero
  parseJSON _ = mzero

data Carryable
  = Banana
  | Trap
  deriving (Eq, Show, Generic)

instance FromJSON Carryable where
  parseJSON (String i) = case i of
    "banana" -> pure Banana
    "trap" -> pure Trap
    _ -> mzero
  parseJSON _ = mzero

instance ToJSON Carryable where
  toJSON Banana = String "banana"
  toJSON Trap = String "trap"
