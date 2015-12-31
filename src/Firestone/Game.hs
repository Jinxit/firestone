{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleContexts       #-}

module Firestone.Game ( Game(..)
                      , PlayerLens(..)
                      , CardLens(..)
                      , p1
                      , p2
                      , makeGame
                      , players
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

import Data.Monoid
import Data.Maybe
import Data.Either.Utils
import Control.Monad.State
import Control.Applicative
import Control.Lens

data Game = Game { gameP1 :: Player
                 , gameP2 :: Player
                 , gameTurn :: Int
                 , gameIdGen :: IdGenerator
                 , gameActive :: Bool
                 } deriving (Show)

makeFields ''Game

type PlayerLens = Lens' Game Player
type CardLens = Traversal' Game Card
type CharacterLens a = Traversal' Game a

(<->) :: Either a b -> Either a b -> Either a b
Left x  <-> Right y = Right y
Left x  <-> Left y  = Left x
Right x <-> _       = Right x

makeGame :: Player -> Player -> Int -> IdGenerator -> Game
makeGame p1 p2 turn idGen = execState start (Game p1 p2 turn idGen True)

play :: Game -> State Game a -> Game
play = flip execState

playerInTurn :: PlayerLens
playerInTurn f game =
    case game^.turn of 0 -> p1 f game
                       1 -> p2 f game

-- TODO: Traversal' Game Player
players :: State Game [Player]
players = do
    p1' <- use p1
    p2' <- use p2
    return [p1', p2']

endTurn :: State Game [Event]
endTurn = do
    turn %= \x -> (x + 1) `mod` 2
    zoom playerInTurn $ do
        activeMinions.traversed.isSleepy .= False
        drawCard
        zoom hero $ do
            increaseMana
            isSleepy .= False
    return []

canAttack :: IsCharacter a
          => PlayerLens -> CharacterLens a -> State Game (Either String Bool)
canAttack p c = do
    player <- use p
    inTurn <- use playerInTurn
    character <- preither c "Invalid attacker"
    return $ and <$> sequence [ C.canAttack <$> character
                              , Right (player == inTurn)
                              ]

preither :: MonadState s m => Getting (First a) s a -> b -> m (Either b a)
preither getter err = do
    maybeValue <- preuse getter
    return $ maybeToEither err maybeValue

isAttackValid :: (IsCharacter a, IsCharacter b)
              => CharacterLens a -> CharacterLens b -> State Game (Either String Bool)
isAttackValid a t = do
    ps <- players
    attacker <- preither a "Invalid attacker"
    target <- preither t "Invalid target"
    return $ and <$> sequence [ C.canAttack <$> attacker
                              , (/=) <$> (ownerOf ps <$> attacker)
                                     <*> (ownerOf ps <$> target)
                              ]

attack :: (IsCharacter a, IsCharacter b)
       => CharacterLens a -> CharacterLens b -> State Game (Either String [Event])
attack attacker target = do
    valid <- isAttackValid attacker target
    case valid of
        Left err    -> return $ Left err
        Right False -> return $ Left "Attack is not valid"
        Right True  -> do
            g1 <- get
            let attackerDamage = g1^?!attacker.attackValue
            target.health -= attackerDamage
            g2 <- get
            let targetDamage = g2^?!target.attackValue
            attacker.health -= targetDamage
            attacker.isSleepy .= True
            return $ Left "boom"

playMinionCard :: CardLens -> Int -> State Game (Either String [Event])
playMinionCard c position = do
    maybeCard <- preuse c
    case maybeCard of
        Nothing   -> do
            return $ Left "Tried to play non-existing minion card"
        Just card -> do
            playerInTurn.hero.mana -= (card^.manaCost)
            playerInTurn.hand %= filter (/= card)
            return $ Left "bam"

start :: State Game ()
start = do
    zoom p1 $ do
        zoom hero increaseMana
        activeMinions.traversed.isSleepy .= False
        replicateM_ 4 drawCard
    zoom p2 $ replicateM_ 4 drawCard

safeHead :: String -> [a] -> Either String a
safeHead str (x:_) = Right x
safeHead str _     = Left str

ownerOf :: IsCharacter a => [Player] -> a -> Either String Player
ownerOf ps c = safeHead ("Ownerless character found: " ++ (c^.uuid)) match
  where
    ownsMinion p = any (\m -> m^.uuid == c^.uuid) (p^.activeMinions)
    ownsHero p = (c^.uuid) == (p^.hero^.uuid)
    match = filter (\p -> (ownsMinion p) || (ownsHero p)) ps
