module Firestone.GameBuilder where

import Firestone.Minion
import Firestone.Player
import Firestone.Hero
import Firestone.Game
import Firestone.IdGenerator

import Control.Monad.State

data GameBuilder = GameBuilder [Player] IdGenerator
                 deriving (Show)

buildGame :: IdGenerator -> State GameBuilder (Game, IdGenerator) -> (Game, IdGenerator)
buildGame idGen buildActions = evalState buildActions (GameBuilder [] idGen)

addPlayer :: String -> State GameBuilder (Game, IdGenerator)
addPlayer heroName = do
    GameBuilder players idGen <- get
    let (hId, idGen2) = create idGen heroName
    let hero = makeHero hId heroName 30 0
    let (pId, idGen3) = create idGen2 "player"
    let player = makePlayer pId hero
    put $ GameBuilder (player:players) idGen3
    build

build :: State GameBuilder (Game, IdGenerator)
build = do
    GameBuilder players idGen <- get
    return $ (Game players 0, idGen)