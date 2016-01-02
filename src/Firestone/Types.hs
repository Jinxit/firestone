{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE LiberalTypeSynonyms    #-}
{-# LANGUAGE ImpredicativeTypes     #-}

module Firestone.Types where

import Firestone.IdGenerator

import Control.Lens
import Control.Monad.State

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

type CardLens = Traversal' Game Card

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

type MinionLens = Traversal' Game Minion

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

data Trigger = Trigger { triggerType :: TriggerType
                       , triggerAction :: (Bool -> Traversal' Game Minion -> State Game ())
                       }

instance Show Trigger where
    show (Trigger t f) = show t

data Game = Game { gameP1 :: Player
                 , gameP2 :: Player
                 , gameTurn :: Int
                 , gameIdGen :: IdGenerator
                 , gameActive :: Bool
                 , gameDebug :: [Int]
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

type CharacterLens a = Traversal' Game a

instance IsCharacter Minion where
    canCharacterAttack m = not (m^.isSleepy) && m^.attackValue > 0
instance IsCharacter Hero where
    canCharacterAttack h = h^.attackValue > 0