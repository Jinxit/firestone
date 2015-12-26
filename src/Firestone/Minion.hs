{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Firestone.Minion ( MinionRace(..)
                        , MinionState(..)
                        , Minion(..)
                        , makeMinion
                        , isSleepy
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
                     , minionAttack :: Int
                     , minionOriginalAttack :: Int
                     , minionHealth :: Int
                     , minionMaxHealth :: Int
                     , minionOriginalHealth :: Int
                     , minionRace :: MinionRace
                     , minionStates :: [MinionState]
                     , minionIsSleepy :: Bool
                     } deriving (Show)

makeFields ''Minion

instance Eq Minion where
    (==) a b = a^.uuid == b^.uuid

instance Ord Minion where
    (<)  a b = a^.uuid <  b^.uuid
    (<=) a b = a^.uuid <= b^.uuid
    (>)  a b = a^.uuid >  b^.uuid
    (>=) a b = a^.uuid >= b^.uuid

makeMinion :: String -> String -> Int -> Int -> MinionRace -> [MinionState] -> Bool -> Minion
makeMinion mId mName mAttack mHealth mRace mStates mIsSleepy = minion
  where
    minion = Minion mId mName mAttack mAttack mHealth mHealth mHealth mRace mStates mIsSleepy
