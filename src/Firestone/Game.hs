{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Firestone.Game ( Game(..)
                      , makeGame
                      , players
                      , playerInTurn
                      , endTurn
                      , play
                      ) where

import Firestone.Event
import Firestone.Player
import Firestone.Error
import Firestone.Minion
import Firestone.Hero
import Firestone.IdGenerator

import Control.Monad.State
import Control.Lens

data Game = Game { gamePlayers :: [Player]
                 , gameTurn :: Int
                 , gameIdGen :: IdGenerator
                 } deriving (Show)

makeFields ''Game

makeGame :: [Player] -> Int -> IdGenerator -> Game
makeGame ps turn idGen = execState start (Game ps turn idGen)

play :: Game -> State Game a -> Game
play = flip execState

start :: State Game ()
start = do
    zoom (players.ix 0) $ do
        zoom hero $ do
            mana .= 1
            maxMana .= 1
    zoom (players.traversed) $ replicateM_ 4 drawCard

playerInTurn :: State Game Player
playerInTurn = do
    game <- get
    return $ game^?!players.ix (game^.turn)

endTurn :: State Game [Event]
endTurn = do
    game <- get
    t <- turn <%= flip mod (length (game^.players)) . (+ 1)
    zoom (players.ix t) $ do
        activeMinions %= wake
        drawCard
        zoom hero increaseMana
    return []
  where
    wake = map (set isSleepy False)
