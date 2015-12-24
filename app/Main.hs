module Main where

import Lib
import Firestone.GameBuilder
import Firestone.Player

import Control.Monad.State

main :: IO ()
main = do
    let players = buildGame $ do
            addPlayer
            addPlayer
    putStrLn (show players)
