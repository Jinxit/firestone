module Main where

import Lib
import Firestone.GameBuilder
import Firestone.Player
import Firestone.Database
import Firestone.IdGenerator

import Control.Monad.State

main :: IO ()
main = do
    let players = buildGame $ do
            addPlayer
            addPlayer
    putStrLn (show players)
    let gen = makeIdGenerator
    let (oasis, gen2) = lookupMinion gen "Oasis Snapjaw"
    putStrLn (show oasis)
