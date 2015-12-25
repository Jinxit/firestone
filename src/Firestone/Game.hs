{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Firestone.Game where

import Firestone.Event
import Firestone.Player

import Control.Monad.State
import Control.Lens

data Game = Game { _gamePlayers :: [Player]
                 , _gameTurn :: Int
                 } deriving (Show)

makeLenses ''Game

makeGame :: [Player] -> Int -> Game
makeGame players turn = Game players turn

playerInTurn :: State Game (Player)
playerInTurn = do
    game <- get
    return $ game^?!gamePlayers.traversed.index (game^.gameTurn)
