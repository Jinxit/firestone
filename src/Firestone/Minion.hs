{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Firestone.Minion ( MinionRace(..)
                        , MinionState(..)
                        , Minion(..)
                        , HasUuid(..)
                        , HasName(..)
                        , HasHealth(..)
                        , HasMaxHealth(..)
                        , HasAttackValue(..)
                        , makeMinion
                        , isSleepy
                        , canAttack
                        ) where

import Control.Lens

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
                     } deriving (Show)

makeFields ''Minion

instance Eq Minion where
    (==) a b = a^.uuid == b^.uuid

instance Ord Minion where
    (<)  a b = a^.timestamp <  b^.timestamp
    (<=) a b = a^.timestamp <= b^.timestamp
    (>)  a b = a^.timestamp >  b^.timestamp
    (>=) a b = a^.timestamp >= b^.timestamp

makeMinion :: String -> String -> Int -> Int -> MinionRace -> [MinionState] -> Bool -> Int -> Minion
makeMinion mId mName mAttack mHealth mRace mStates mIsSleepy mTimestamp = minion
  where
    minion = Minion mId mName mAttack mAttack mHealth mHealth mHealth mRace mStates mIsSleepy mTimestamp

canAttack :: Minion -> Bool
canAttack m = not (m^.isSleepy) && m^.attackValue > 0
