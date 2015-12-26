{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Firestone.Game where

import Firestone.Event
import Firestone.Player
import Firestone.Error
import Firestone.Minion
import Firestone.Hero

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

endTurn :: State Game ([Event])
endTurn = do
    game <- get
    gameTurn %= flip mod (length (game^.gamePlayers)) . (+ 1)
    zoom (gamePlayers.traversed.index (game^.gameTurn)) $ do
        playerActiveMinions %= wake
        zoom playerHero $ do
            heroMaxMana %= min 10 . (+ 1)
            heroMana <~ use heroMaxMana
    return []
  where
    wake = map (set minionIsSleepy False)
