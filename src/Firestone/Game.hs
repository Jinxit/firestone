{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE LiberalTypeSynonyms    #-}
{-# LANGUAGE ImpredicativeTypes     #-}

module Firestone.Game ( makeGame
                      , players
                      , ownerOf
                      , positionOf
                      , play
                      , playerInTurn
                      , endTurn
                      , canAttack
                      , isAttackValid
                      , attack
                      , playMinionCard
                      , prerror
                      ) where

import Firestone.Types
import Firestone.Player
import Firestone.Hero
import Firestone.IdGenerator
import Firestone.Utils
import {-# SOURCE #-} Firestone.Database

import Control.Exception.Base
import Control.Lens
import Control.Monad.State
import Data.List
import Data.Maybe

makeGame :: Player -> Player -> Int -> IdGenerator -> Game
makeGame p1 p2 turn idGen = execState start (Game p1 p2 turn idGen True)

players :: State Game [Player]
players = mapM use [p1, p2]

play :: Game -> State Game a -> Game
play = flip execState

playerInTurn :: PlayerLens
playerInTurn f game =
    case game^.turn of 0 -> p1 f game
                       1 -> p2 f game

endTurn :: State Game [Event]
endTurn = do
    turn %= \x -> (x + 1) `mod` 2
    zoom playerInTurn $ do
        activeMinions.traversed.isSleepy .= False
        drawCard
        zoom hero $ do
            increaseMana
            isSleepy .= False
    endAction
    return []

canAttack :: IsCharacter a
          => PlayerLens -> CharacterLens a -> Game -> Bool
canAttack player character game = canCharacterAttack (game^?!character)
                               && (game^?!player) == (game^?!playerInTurn)

isAttackValid :: (IsCharacter a, IsCharacter b)
              => CharacterLens a -> CharacterLens b -> Game -> Bool
isAttackValid a t game = canCharacterAttack attacker
                       && (game^?!(ownerOf attacker)) /= (game^?!(ownerOf target))
  where
    attacker = game^?!a
    target   = game^?!t

isAttackValidM :: (IsCharacter a, IsCharacter b)
               => CharacterLens a -> CharacterLens b -> State Game Bool
isAttackValidM a t = do
    game <- get
    return $ isAttackValid a t game

attack :: (IsCharacter a, IsCharacter b, Triggerable a, Triggerable b)
       => CharacterLens a -> CharacterLens b -> State Game [Event]
attack attacker target = do
    valid <- isAttackValidM attacker target
    case valid of
        False -> error "Attack is not valid"
        True  -> do
            attackerAttack <- unuse (attacker.attackValue)
            damage target attackerAttack
            targetAttack <- unuse (target.attackValue)
            damage attacker targetAttack
            attacker.isSleepy .= True
            endAction
            return $ []

playMinionCard :: CardLens -> Int -> State Game [Event]
playMinionCard c position = do
    maybeCard <- preuse c
    case maybeCard of
        Nothing   -> do
            error "Tried to play non-existing minion card"
        Just card -> do
            p <- use playerInTurn
            cardOwner <- use (ownerOf card)
            assert (p == cardOwner) $ return ()
            playerInTurn.hero.mana -= (card^.manaCost)
            playerInTurn.hand %= filter (/= card)
            gen1 <- use idGen
            let (minion, gen2) = lookupMinion gen1 (card^.name)
            idGen .= gen2
            zoom playerInTurn $ summonMinionAt position minion
            endAction
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

endAction :: State Game [Event]
endAction = do
    removeDeadMinions
    checkGameOver

    return []