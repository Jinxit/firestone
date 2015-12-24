module Firestone.GameBuilder where

import Firestone.Minion
import Firestone.Player
import Firestone.Hero

import Control.Monad.State

data GameBuilder = GameBuilder [Player]
                 deriving (Show)

buildGame :: State GameBuilder [Player] -> [Player]
buildGame buildActions = evalState buildActions (GameBuilder [])

addPlayer :: State GameBuilder [Player]
addPlayer = do
    GameBuilder players <- get
    put $ GameBuilder (player:players)
    build
  where
    hero = makeHero "hero_id" "hero_name" 30 0
    player = makePlayer "player_id" hero

build :: State GameBuilder [Player]
build = do
    GameBuilder players <- get
    return players