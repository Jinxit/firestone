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
    let (game, idGen2) = buildGame idGen $ do
            addPlayer "Jaina"
            addPlayer "Thrall"
    putStrLn (show game)
    let (oasis, idGen3) = lookupMinion idGen2 "Oasis Snapjaw"
    putStrLn (show oasis)
