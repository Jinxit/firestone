{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleContexts       #-}

module Firestone.Gameplay ( makeGame
                          , p1
                          , p2
                          , active
                          , playerInTurn
                          , endTurn
                          , play
                          , attack
                          , canAttack
                          , isAttackValid
                          , playMinionCard
                          ) where

import Firestone.Event
import Firestone.Player
import Firestone.Error
import Firestone.Minion hiding (canAttack, attack)
import qualified Firestone.Minion as M
import Firestone.Hero hiding (canAttack, attack)
import Firestone.IdGenerator
import Firestone.Character hiding (canAttack)
import qualified Firestone.Character as C
import Firestone.Card
import Firestone.Database
import Firestone.Game

import Data.Monoid
import Data.Maybe
import Data.Either.Utils
import Control.Monad.State
import Control.Applicative
import Control.Lens
import Control.Exception.Base

makeGame :: Player -> Player -> Int -> IdGenerator -> Game
makeGame p1 p2 turn idGen = execState start (Game p1 p2 turn idGen True)

play :: Game -> State Game a -> Game
play = flip execState

playerInTurn :: PlayerLens
playerInTurn f game =
    case game^.turn of 0 -> p1 f game
                       1 -> p2 f game

players :: State Game [Player]
players = mapM use [p1, p2]

endTurn :: State Game [Event]
endTurn = do
    turn %= \x -> (x + 1) `mod` 2
    zoom playerInTurn $ do
        activeMinions.traversed.isSleepy .= False
        drawCard
        zoom hero $ do
            increaseMana
            isSleepy .= False
    removeDeadMinions
    checkGameOver
    return []

canAttack :: IsCharacter a
          => PlayerLens -> CharacterLens a -> State Game Bool
canAttack p c = do
    player <- use p
    inTurn <- use playerInTurn
    character <- prerror c "Invalid attacker"
    return $ C.canAttack character
          && player == inTurn

prerror :: MonadState s m => Getting (First a) s a -> String -> m a
prerror getter err = do
    maybeValue <- preuse getter
    case maybeValue of
        Just x  -> return x
        Nothing -> error err

isAttackValid :: (IsCharacter a, IsCharacter b)
              => CharacterLens a -> CharacterLens b -> State Game Bool
isAttackValid a t = do
    ps <- players
    attacker <- prerror a "Invalid attacker"
    target <- prerror t "Invalid target"
    return $ C.canAttack attacker
          && (ownerOf ps attacker) /= (ownerOf ps target)

attack :: (IsCharacter a, IsCharacter b)
       => CharacterLens a -> CharacterLens b -> State Game [Event]
attack attacker target = do
    valid <- isAttackValid attacker target
    case valid of
        False -> error "Attack is not valid"
        True  -> do
            g1 <- get
            target.health -= (g1^?!attacker.attackValue)
            g2 <- get
            attacker.health -= (g2^?!target.attackValue)
            attacker.isSleepy .= True
            removeDeadMinions
            checkGameOver
            return $ []

insertAt :: Int -> a -> [a] -> [a]
insertAt i x xs = ls ++ (x:rs)
  where
    (ls, rs) = splitAt i xs

playMinionCard :: CardLens -> Int -> State Game [Event]
playMinionCard c position = do
    maybeCard <- preuse c
    case maybeCard of
        Nothing   -> do
            error "Tried to play non-existing minion card"
        Just card -> do
            p <- use playerInTurn
            ps <- players
            assert (p == (ownerOf ps card)) $ return ()
            playerInTurn.hero.mana -= (card^.manaCost)
            playerInTurn.hand %= filter (/= card)
            gen1 <- use idGen
            let (minion, gen2) = lookupMinion gen1 (card^.name)
            idGen .= gen2
            playerInTurn.activeMinions %= insertAt position minion
            removeDeadMinions
            checkGameOver
            return []

start :: State Game ()
start = do
    zoom p1 $ do
        zoom hero increaseMana
        activeMinions.traversed.isSleepy .= False
        replicateM_ 4 drawCard
    zoom p2 $ replicateM_ 4 drawCard

checkGameOver :: State Game ()
checkGameOver = do
    p1hp <- use (p1.hero.health)
    p2hp <- use (p2.hero.health)
    active .= (p1hp > 0 && p2hp > 0)

removeDeadMinions :: State Game [Event]
removeDeadMinions = do
    forM [p1, p2] $ flip zoom $ do
        activeMinions %= filter (\m -> m^.health > 0)
    return []

ownerOf :: HasUuid a String => [Player] -> a -> Player
ownerOf ps u = case length match of
    0         -> error ("Ownerless thing found: " ++ (u^.uuid))
    otherwise -> head match
  where
    ownsMinion p = any (\m -> m^.uuid == u^.uuid) (p^.activeMinions)
    ownsHero p = (u^.uuid) == (p^.hero^.uuid)
    ownsCard p = any (\c -> c^.uuid == u^.uuid) (p^.hand)
    match = filter (\p -> ownsMinion p
                       || ownsHero p
                       || ownsCard p
                   ) ps
