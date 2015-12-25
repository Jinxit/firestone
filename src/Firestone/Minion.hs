{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Firestone.Minion where

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

data Minion = Minion { _minionId :: String
                     , _minionName :: String
                     , _minionHealth :: Int
                     , _minionMaxHealth :: Int
                     , _minionOriginalHealth :: Int
                     , _minionAttack :: Int
                     , _minionOriginalAttack :: Int
                     , _minionRace :: MinionRace
                     , _minionStates :: [MinionState]
                     , _minionIsSleepy :: Bool
                     } deriving (Show)

makeLenses ''Minion

instance Eq Minion where
    (==) a b = a^.minionId == b^.minionId

instance Ord Minion where
    (<)  a b = a^.minionId <  b^.minionId
    (<=) a b = a^.minionId <= b^.minionId
    (>)  a b = a^.minionId >  b^.minionId
    (>=) a b = a^.minionId >= b^.minionId

makeMinion :: String -> String -> Int -> Int -> MinionRace -> [MinionState] -> Bool -> Minion
makeMinion mId mName mAttack mHealth mRace mStates mIsSleepy = minion
  where
    minion = Minion mId mName mAttack mAttack mHealth mHealth mHealth mRace mStates mIsSleepy
