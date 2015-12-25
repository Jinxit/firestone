module Firestone.Game where

import Firestone.Event
import Firestone.Player

data Game = Game { gamePlayers :: [Player]
                 , gameTurn :: Int
                 } deriving (Show)

