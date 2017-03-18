{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE LiberalTypeSynonyms    #-}
{-# LANGUAGE ImpredicativeTypes     #-}
{-# LANGUAGE DeriveFunctor          #-}


module Firestone.Types where

import Firestone.IdGenerator
import Firestone.Utils

import Control.Lens
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

data Side = Before | After deriving (Show, Eq)

data Position = Index Int
              | Last
              | Adjacent MinionLens Side

data Action next = DamageMinion MinionLens Int next
                 | SpawnMinion PlayerLens String Position next
                 | GetMinions PlayerLens (MinionLens -> next)

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

type CharacterLens a = Traversal' Game a

instance IsCharacter Minion where
    canCharacterAttack m = not (m^.isSleepy) && m^.attackValue > 0
instance IsCharacter Hero where
    canCharacterAttack h = h^.attackValue > 0

-- came from Game.hs
ownerOf :: HasUuid a String => a -> PlayerLens
ownerOf u f g =
    case ownsAny p1 of
        True  -> p1 f g
        False -> case ownsAny p2 of
                    True  -> p2 f g
                    False -> error ("Ownerless thing found: " ++ (u^.uuid))
  where
    ownsMinion p = any (\m -> m^.uuid == u^.uuid) (p^.activeMinions)
    ownsHero p = (u^.uuid) == (p^.hero^.uuid)
    ownsCard p = any (\c -> c^.uuid == u^.uuid) (p^.hand)
    ownsAny p = ownsMinion (g^.p)
             || ownsHero (g^.p)
             || ownsCard (g^.p)

positionOf :: Minion -> State Game Int
positionOf minion = do
    owner <- use (ownerOf minion)
    let pos = (minion `elemIndex` (owner^.activeMinions))
    return $ fromMaybe (-1) pos

-- came from Script.hs
interpret :: Script () -> State Game [Event]
interpret script = case script of 
    Free (DamageMinion minion amount next) -> do
        damage minion amount
        interpret next
    Free (SpawnMinion player name pos next) -> do
        index <- case pos of
                        (Index i) -> return i
                        Last -> return $ views activeMinions length
                        (Adjacent minion side) -> do
                            i2 <- positionOf minion
                            return (case side of
                                        Left -> i2
                                        Right -> i2 + 1)

        gen1 <- use idGen
        let (minion, gen2) = lookupMinion gen1 name
        idGen .= gen2
        zoom player $ summonMinionAt index minion
        interpret next 
    Free (GetMinions player next) -> do
        let minions = player^.activeMinions
        interpret (next minions)
    Pure _ -> return []

-- came from Trigger.hs
class Triggerable c where
    trigger :: TriggerType -> ATraversal' Game c -> State Game [Event]

instance Triggerable Minion where
    trigger t aC = do
        let c = cloneTraversal aC
        mId <- prerror (c.uuid) "Invalid character sent to trigger"
        let isSame minion = minion^.uuid == mId
        p1Minions <- use (p1.activeMinions)
        p2Minions <- use (p2.activeMinions)
        let allMinions = sort (p1Minions ++ p2Minions)
        let allTriggers = map (\minion -> (isSame minion, minion^.triggers)) allMinions
        let matchingTriggers = map (\(same, triggers) -> (same, filter ((== t) . triggerType) triggers)) allTriggers
        let actions = concat $ map (\(same, triggers) -> map (\trig -> (triggerAction trig) same) triggers) matchingTriggers
        forM_ actions $ \action -> interpret $ action (views aC ownerOf) (cloneTraversal aC)
        return []

instance Triggerable Hero where
    trigger t aC = return []

-- came from Character.hs
damage :: (IsCharacter c, Triggerable c) => CharacterLens c -> Int -> State Game [Event]
damage c d = do
    c.health -= d
    case d > 0 of
        True  -> trigger MinionDamaged c
        False -> return []
    return []
