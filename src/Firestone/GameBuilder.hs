{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Firestone.GameBuilder ( buildGame
                             , baseGame
                             , setMaxHealth
                             , setStartingMana
                             , setActiveMinions
                             , setDeck
                             ) where

import Firestone.Types
import Firestone.IdGenerator
import Firestone.Database
import Firestone.Game

import Control.Monad.State
import Control.Lens

data GameBuilder = GameBuilder { gameBuilderP1 :: Player
                               , gameBuilderP2 :: Player
                               , gameBuilderIdGen :: IdGenerator
                               }

makeFields ''GameBuilder

buildGame :: State GameBuilder a -> Game
buildGame buildActions = flip evalState makeGameBuilder $ do
    buildActions
    gb <- get
    return $ makeGame (gb^.p1) (gb^.p2) 0 (gb^.idGen)

baseGame :: Game
baseGame = buildGame $ return ()

makeGameBuilder :: GameBuilder
makeGameBuilder = GameBuilder player1 player2 makeIdGenerator
  where
    idGen1 = makeIdGenerator
    (player1, idGen2) = createPlayer idGen1
    (player2, idGen3) = createPlayer idGen2

createPlayer :: IdGenerator -> (Player, IdGenerator)
createPlayer idGen1 = (makePlayer pId hero, idGen3)
  where
    (hId, _, idGen2) = create idGen1 "hero"
    hero = makeHero hId "hero" 30 0
    (pId, _, idGen3) = create idGen2 "player"

playerAt :: Int -> Lens' GameBuilder Player
playerAt i f gb = case i of 1 -> p1 f gb
                            2 -> p2 f gb

heroAt :: Int -> Lens' GameBuilder Hero
heroAt i = playerAt i.hero

setMaxHealth :: Int -> Int -> State GameBuilder ()
setMaxHealth i newHealth = do
    zoom (heroAt i) $ do
        health .= newHealth
        maxHealth .= newHealth

setStartingMana :: Int -> Int -> State GameBuilder ()
setStartingMana i newMana = do
    zoom (heroAt i) $ do
        mana .= newMana
        maxMana .= newMana

setActiveMinions :: Int -> [String] -> State GameBuilder ()
setActiveMinions i names = do
    gb <- get
    let (newMinions, newGen) = lookupMinions (gb^.idGen) names
    playerAt i.activeMinions .= newMinions
    idGen .= newGen

setDeck :: Int -> [String] -> State GameBuilder ()
setDeck i names = do
    gb <- get
    let (newCards, newGen) = lookupCards (gb^.idGen) names
    playerAt i.deck.cards .= newCards
    idGen .= newGen
