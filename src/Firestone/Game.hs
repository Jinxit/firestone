{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleContexts       #-}

module Firestone.Game ( Game(..)
                      , makeGame
                      , players
                      , playerInTurn
                      , endTurn
                      , play
                      ) where

import Firestone.Event
import Firestone.Player
import Firestone.Error
import Firestone.Minion hiding (canAttack)
import qualified Firestone.Minion as M
import Firestone.Hero hiding (canAttack)
import Firestone.IdGenerator
import Firestone.Character hiding (canAttack)
import qualified Firestone.Character as C

import Data.Monoid
import Data.Maybe
import Control.Monad.State
import Control.Applicative
import Control.Lens

data Game = Game { gamePlayers :: [Player]
                 , gameTurn :: Int
                 , gameIdGen :: IdGenerator
                 } deriving (Show)

makeFields ''Game

(<->) :: Either a b -> Either a b -> Either a b
Left x  <-> Right y = Right y
Left x  <-> Left y  = Left x
Right x <-> _       = Right x

makeGame :: [Player] -> Int -> IdGenerator -> Game
makeGame ps turn idGen = execState start (Game ps turn idGen)

play :: Game -> State Game a -> Game
play = flip execState

start :: State Game ()
start = do
    zoom (players.ix 0) $ do
        zoom hero $ do
            mana .= 1
            maxMana .= 1
    zoom (players.traversed) $ replicateM_ 4 drawCard

playerInTurn :: State Game Player
playerInTurn = do
    game <- get
    return $ game^?!players.ix (game^.turn)

endTurn :: State Game [Event]
endTurn = do
    game <- get
    t <- turn <%= \x -> (x + 1) `mod` (length (game^.players))
    zoom (players.ix t) $ do
        activeMinions %= wake
        drawCard
        zoom hero increaseMana
    return []
  where
    wake = map (set isSleepy False)

sameUuid :: HasUuid a String => String -> a -> Bool
sameUuid charId char = charId == (char^.uuid)

safeHead :: String -> [a] -> Either String a
safeHead str (x:_) = Right x
safeHead str _     = Left str

getHero :: String -> [Player] -> Either String Hero
getHero charId ps = safeHead ("No hero with UUID " ++ charId) match
  where
    match = filter (sameUuid charId) $ map (view hero) ps

getMinion :: String -> [Player] -> Either String Minion
getMinion charId ps = safeHead ("No minion with UUID " ++ charId) match
  where
    match = filter (sameUuid charId) $ concatMap (view activeMinions) ps

getCharacter :: String -> [Player] -> Either String Character
getCharacter charId ps = (CHero <$> h) <-> (CMinion <$> m)
  where
    h = getHero charId ps
    m = getMinion charId ps

ownerOf :: [Player] -> Character -> Either String Player
ownerOf ps (CMinion m) = safeHead ("Ownerless minion found: " ++ (m^.uuid)) match
  where
    ownsMinion p = m `elem` (p^.activeMinions)
    match = filter ownsMinion ps

ownerOf ps (CHero h) = safeHead ("Ownerless hero found: " ++ (h^.uuid)) match
  where
    ownsHero p = h == (p^.hero)
    match = filter ownsHero ps

canAttack :: Player -> Minion -> State Game (Bool)
canAttack p m = do
    currentPlayer <- playerInTurn
    return $ M.canAttack m && currentPlayer == p

isAttackValid :: String -> String -> State Game (Either String Bool)
isAttackValid attackerId targetId = do
    ps <- use players
    let attacker = getCharacter attackerId ps
    let target = getCharacter targetId ps
    return $ and <$> sequence [ C.canAttack <$> attacker
                              , (/=) <$> (ownerOf ps <$> attacker)
                                     <*> (ownerOf ps <$> target)
                              ]
