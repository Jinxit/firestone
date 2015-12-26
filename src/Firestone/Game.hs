{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Firestone.Game ( Game(..)
                      , makeGame
                      , playerInTurn
                      , endTurn
                      ) where

import Firestone.Event
import Firestone.Player
import Firestone.Error
import Firestone.Minion
import Firestone.Hero

import Control.Monad.State
import Control.Lens

data Game = Game { gamePlayers :: [Player]
                 , gameTurn :: Int
                 } deriving (Show)

makeFields ''Game

makeGame :: [Player] -> Int -> Game
makeGame ps turn = execState start (Game ps turn)

start :: State Game ()
start = do
    zoom (players.ix 0.hero) $ do
        mana .= 1
        maxMana .= 1

playerInTurn :: State Game Player
playerInTurn = do
    game <- get
    return $ game^?!players.traversed.index (game^.turn)

endTurn :: State Game [Event]
endTurn = do
    game <- get
    turn %= flip mod (length (game^.players)) . (+ 1)
    zoom (players.traversed.index (game^.turn)) $ do
        activeMinions %= wake
        zoom hero $ do
            maxMana %= min 10 . (+ 1)
            mana <~ use maxMana
    return []
  where
    wake = map (set isSleepy False)
