module Firestone.Minion where

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

data Minion = Minion { minionId :: String
                     , minionName :: String
                     , minionHealth :: Int
                     , minionMaxHealth :: Int
                     , minionOriginalHealth :: Int
                     , minionAttack :: Int
                     , minionOriginalAttack :: Int
                     , minionRace :: MinionRace
                     , minionStates :: [MinionState]
                     , minionIsSleepy :: Bool
                     } deriving (Show)

instance Eq Minion where
    (==) a b = minionId a == minionId b

instance Ord Minion where
    (<)  a b = minionId a <  minionId b
    (<=) a b = minionId a <= minionId b
    (>)  a b = minionId a >  minionId b
    (>=) a b = minionId a >= minionId b

makeMinion :: String -> String -> Int -> Int -> MinionRace -> [MinionState] -> Bool -> Minion
makeMinion mId mName mAttack mHealth mRace mStates mIsSleepy = minion
  where
    minion = Minion mId mName mAttack mAttack mHealth mHealth mHealth mRace mStates mIsSleepy
