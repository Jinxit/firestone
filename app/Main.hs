module Main where

import Lib
import Firestone.GameBuilder
import Firestone.Player
import Firestone.Database
import Firestone.IdGenerator

import Control.Monad.State

main :: IO ()
main = do
    let idGen = makeIdGenerator
    let (game, idGen2) = buildGame idGen lookupMinions $ do
            addPlayer "Jaina"
            addPlayer "Thrall"
            setMaxHealth 1 20
    putStrLn (show game)
