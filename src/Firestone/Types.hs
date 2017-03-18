{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE LiberalTypeSynonyms    #-}
{-# LANGUAGE DeriveFunctor          #-}


module Firestone.Types where

import Firestone.IdGenerator
import Firestone.Utils

import Control.Lens
import Control.Lens.Reified
import Control.Monad.State
import Control.Monad.Free
import Data.List
import Data.Maybe

data Deck = Deck { deckCards :: [Card]
                 } deriving (Show)

data CardType = MinionCard
              | SpellCard
              | WeaponCard
              deriving (Show, Eq)

data Card = Card { cardUuid :: String
                 , cardName :: String
                 , cardManaCost :: Int
                 , cardOriginalManaCost :: Int
                 , cardAttackValue :: Maybe Int
                 , cardHealth :: Maybe Int
                 , cardCardType :: CardType
                 , cardDescription :: String
                 , cardIsTargeting :: Bool
                 } deriving (Show)

type CardLens = Lens' Game Card

makeCard :: String -> String -> Int -> Maybe Int -> Maybe Int -> CardType
         -> String -> Bool -> Card
makeCard cId cName cMana cAttack cHealth cType cDesc cIsTargeting =
    Card cId cName cMana cMana cAttack cHealth cType cDesc cIsTargeting

data Hero = Hero { heroUuid :: String
                 , heroName :: String
                 , heroHealth :: Int
                 , heroMaxHealth :: Int
                 , heroMana :: Int
                 , heroMaxMana :: Int
                 , heroAttackValue :: Int
                 , heroIsSleepy :: Bool
                 } deriving (Show)

makeHero :: String -> String -> Int -> Int -> Hero
makeHero hId hName hHp hMana = Hero hId hName hHp hHp hMana hMana 0 False

data MinionRace = Beast
                | Demon
                | Mech
                | Murloc
                | None
                deriving (Show, Eq)

data MinionState = DivineShield
                 | Freeze
                 | Immune
                 | Stealth
                 | Silenced
                 | Taunt
                 | Windfury
                 deriving (Show, Eq)

data Minion = Minion { minionUuid :: String
                     , minionName :: String
                     , minionAttackValue :: Int
                     , minionOriginalAttackValue :: Int
                     , minionHealth :: Int
                     , minionMaxHealth :: Int
                     , minionOriginalHealth :: Int
                     , minionRace :: MinionRace
                     , minionStates :: [MinionState]
                     , minionIsSleepy :: Bool
                     , minionTimestamp :: Int
                     , minionTriggers :: [Trigger]
                     } deriving (Show)

type MinionLens = Lens' Game Minion
type ReifiedMinionLens = ReifiedLens' Game Minion
type ReifiedMinionTraversal = ReifiedTraversal' Game Minion

makeMinion :: String -> String -> Int -> Int -> MinionRace
           -> [MinionState] -> Bool -> Int -> [Trigger] -> Minion
makeMinion mId mName mAttack mHealth mRace mStates mIsSleepy mTimestamp mTriggers = minion
  where
    minion = Minion mId mName mAttack mAttack mHealth mHealth mHealth
                    mRace mStates mIsSleepy mTimestamp mTriggers

data Player = Player { playerUuid :: String
                     , playerHero :: Hero
                     , playerHand :: [Card]
                     , playerActiveMinions :: [Minion]
                     , playerDeck :: Deck
                     , playerFatigue :: Int
                     } deriving (Show)

type PlayerLens = Lens' Game Player

makePlayer :: String -> Hero -> Player
makePlayer pId pHero = Player pId pHero [] [] (Deck []) 1

data TriggerType = MinionDamaged
                 deriving (Show, Eq)

data Side = Before | After deriving (Show, Eq)

data Position = Index Int
              | Last
              | Adjacent MinionLens Side

data Action next = DamageMinion MinionLens Int next
                 | SpawnMinion PlayerLens String Position next
                 | GetMinions PlayerLens (ReifiedMinionTraversal -> next)

instance Functor Action where
    fmap f (DamageMinion minion amount next) = DamageMinion minion amount (f next)
    fmap f (SpawnMinion player string pos next) = SpawnMinion player string pos (f next)
    fmap f (GetMinions player g) = GetMinions player (f . g)

type Script = Free Action

data Trigger = Trigger { triggerType :: TriggerType
                       , triggerAction :: (Bool -> PlayerLens -> MinionLens -> Script ())
                       }

instance Show Trigger where
    show (Trigger t f) = show t

data Game = Game { gameP1 :: Player
                 , gameP2 :: Player
                 , gameTurn :: Int
                 , gameIdGen :: IdGenerator
                 , gameActive :: Bool
                 } deriving (Show)

data Event = Attack   { eventAttacker :: Minion
                      , eventTarget :: Minion
                      }
           | PlayCard { eventCard :: Card
                      , eventTarget :: Minion
                      }
            deriving (Show)

makeFields ''Deck
makeFields ''Card
makeFields ''Hero
makeFields ''Minion
makeFields ''Player
makeFields ''Game
makeFields ''Event

instance Eq Card where
    (==) a b = a^.uuid == b^.uuid

instance Eq Hero where
    (==) a b = a^.uuid == b^.uuid

instance Eq Minion where
    (==) a b = a^.uuid == b^.uuid

instance Ord Minion where
    (<)  a b = a^.timestamp <  b^.timestamp
    (<=) a b = a^.timestamp <= b^.timestamp
    (>)  a b = a^.timestamp >  b^.timestamp
    (>=) a b = a^.timestamp >= b^.timestamp

instance Eq Player where
    (==) a b = a^.uuid == b^.uuid

class ( HasUuid c String
      , HasName c String
      , HasHealth c Int
      , HasMaxHealth c Int
      , HasAttackValue c Int
      , HasIsSleepy c Bool
      ) => IsCharacter c where
    canCharacterAttack :: c -> Bool

type CharacterLens a = Lens' Game a

instance IsCharacter Minion where
    canCharacterAttack m = not (m^.isSleepy) && m^.attackValue > 0
instance IsCharacter Hero where
    canCharacterAttack h = h^.attackValue > 0

class Triggerable c where
    trigger :: HasUuid c String => TriggerType -> c -> State Game [Event]
