{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Firestone.GameBuilder where

import Firestone.Minion
import Firestone.Player
import Firestone.Hero
import Firestone.Game
import Firestone.IdGenerator

import Control.Monad.State
import Control.Lens

data GameBuilder = GameBuilder { _gbPlayers :: [Player]
                               , _gbIdGen :: IdGenerator
                               } deriving (Show)

makeLenses ''GameBuilder

buildGame :: IdGenerator -> State GameBuilder (Game, IdGenerator) -> (Game, IdGenerator)
buildGame idGen buildActions = evalState buildActions (GameBuilder [] idGen)

addPlayer :: String -> State GameBuilder (Game, IdGenerator)
addPlayer heroName = do
    gb <- get
    let (hId, idGen2) = create (gb^.gbIdGen) heroName
    let hero = makeHero hId heroName 30 1
    let (pId, idGen3) = create idGen2 "player"
    let player = makePlayer pId hero
    gbPlayers %= (|> player)
    gbIdGen .= idGen3
    build

setMaxHealth :: Int -> Int -> State GameBuilder (Game, IdGenerator)
setMaxHealth i health = do
    zoom (gbPlayers.traversed.index (i - 1).playerHero) $ do
        heroHealth .= health
        heroMaxHealth .= health
    build

setStartingMana :: Int -> Int -> State GameBuilder (Game, IdGenerator)
setStartingMana i mana = do
    zoom (gbPlayers.traversed.index (i - 1).playerHero) $ do
        heroMana .= mana
        heroMaxMana .= mana
    build

build :: State GameBuilder (Game, IdGenerator)
build = do
    gb <- get
    return $ (Game (gb^.gbPlayers) 0, gb^.gbIdGen)
