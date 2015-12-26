{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Firestone.GameBuilder ( buildGame
                             , addPlayer
                             , addPlayers
                             , setMaxHealth
                             , setStartingMana
                             , setActiveMinions
                             , setDeck
                             ) where

import Firestone.Minion
import Firestone.Card
import Firestone.Player
import Firestone.Hero
import Firestone.Game (Game, makeGame)
import Firestone.Deck
import Firestone.IdGenerator
import Firestone.Database

import Control.Monad.State
import Control.Lens

data GameBuilder = GameBuilder { gameBuilderPlayers :: [Player]
                               , gameBuilderIdGen :: IdGenerator
                               }

makeFields ''GameBuilder

buildGame :: State GameBuilder Game -> Game
buildGame buildActions =
    evalState buildActions (GameBuilder [] makeIdGenerator)

addPlayer :: String -> State GameBuilder Game
addPlayer heroName = do
    gb <- get
    let (hId, idGen2) = create (gb^.idGen) heroName
    let hero = makeHero hId heroName 30 0
    let (pId, idGen3) = create idGen2 "player"
    let player = makePlayer pId hero
    players %= (|> player)
    idGen .= idGen3
    build

addPlayers :: Int -> State GameBuilder Game
addPlayers n = replicateM_ n (addPlayer "hero") >> build

playerAt :: Int -> Traversal' GameBuilder Player
playerAt i = players.traversed.index (i - 1)

heroAt :: Int -> Traversal' GameBuilder Hero
heroAt i = playerAt i.hero

setMaxHealth :: Int -> Int -> State GameBuilder Game
setMaxHealth i newHealth = do
    zoom (heroAt i) $ do
        health .= newHealth
        maxHealth .= newHealth
    build

setStartingMana :: Int -> Int -> State GameBuilder Game
setStartingMana i newMana = do
    zoom (heroAt i) $ do
        mana .= newMana
        maxMana .= newMana
    build

setActiveMinions :: Int -> [String] -> State GameBuilder Game
setActiveMinions i names = do
    gb <- get
    let (newMinions, newGen) = lookupMinions (gb^.idGen) names
    playerAt i.activeMinions .= newMinions
    idGen .= newGen
    build

setDeck :: Int -> [String] -> State GameBuilder Game
setDeck i names = do
    gb <- get
    let (newCards, newGen) = lookupCards (gb^.idGen) names
    playerAt i.deck.cards .= newCards
    idGen .= newGen
    build

build :: State GameBuilder Game
build = do
    gb <- get
    return $ makeGame (gb^.players) 0 (gb^.idGen)
