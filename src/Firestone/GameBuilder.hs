{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Firestone.GameBuilder ( buildGame
                             , addPlayer
                             , setMaxHealth
                             , setStartingMana
                             , setActiveMinions
                             , setDeck
                             ) where

import Firestone.Minion
import Firestone.Card
import Firestone.Player
import Firestone.Hero
import Firestone.Game
import Firestone.Deck
import Firestone.IdGenerator

import Control.Monad.State
import Control.Lens

data GameBuilder = GameBuilder { gameBuilderPlayers :: [Player]
                               , gameBuilderIdGen :: IdGenerator
                               , gameBuilderLookupMinions :: (IdGenerator -> [String]
                                                          -> ([Minion], IdGenerator))
                               , gameBuilderLookupCards :: (IdGenerator -> [String]
                                                        -> ([Card], IdGenerator))
                               }

makeFields ''GameBuilder

buildGame :: IdGenerator
          -> (IdGenerator -> [String] -> ([Minion], IdGenerator))
          -> (IdGenerator -> [String] -> ([Card], IdGenerator))
          -> State GameBuilder (Game, IdGenerator) -> (Game, IdGenerator)
buildGame idGen lookupMinions lookupCards buildActions =
    evalState buildActions (GameBuilder [] idGen lookupMinions lookupCards)

addPlayer :: String -> State GameBuilder (Game, IdGenerator)
addPlayer heroName = do
    gb <- get
    let (hId, idGen2) = create (gb^.idGen) heroName
    let hero = makeHero hId heroName 30 0
    let (pId, idGen3) = create idGen2 "player"
    let player = makePlayer pId hero
    players %= (|> player)
    idGen .= idGen3
    build

playerAt :: Int -> Traversal' GameBuilder Player
playerAt i = players.traversed.index (i - 1)

heroAt :: Int -> Traversal' GameBuilder Hero
heroAt i = playerAt i.hero

setMaxHealth :: Int -> Int -> State GameBuilder (Game, IdGenerator)
setMaxHealth i newHealth = do
    zoom (heroAt i) $ do
        health .= newHealth
        maxHealth .= newHealth
    build

setStartingMana :: Int -> Int -> State GameBuilder (Game, IdGenerator)
setStartingMana i newMana = do
    zoom (heroAt i) $ do
        mana .= newMana
        maxMana .= newMana
    build

setActiveMinions :: Int -> [String] -> State GameBuilder (Game, IdGenerator)
setActiveMinions i names = do
    gb <- get
    let (newMinions, newGen) = (gb^.lookupMinions) (gb^.idGen) names
    playerAt i.activeMinions .= newMinions
    idGen .= newGen
    build

setDeck :: Int -> [String] -> State GameBuilder (Game, IdGenerator)
setDeck i names = do
    gb <- get
    let (newCards, newGen) = (gb^.lookupCards) (gb^.idGen) names
    playerAt i.deck.cards .= newCards
    idGen .= newGen
    build

build :: State GameBuilder (Game, IdGenerator)
build = do
    gb <- get
    return (makeGame (gb^.players) 0, gb^.idGen)
