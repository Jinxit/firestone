{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Firestone.Game where

import Firestone.Event
import Firestone.Player

import Control.Lens

data Game = Game { _gamePlayers :: [Player]
                 , _gameTurn :: Int
                 } deriving (Show)

makeLenses ''Game