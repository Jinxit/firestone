{-# LANGUAGE RankNTypes             #-}

module Firestone.Database ( lookupMinion
                          , lookupMinionM
                          , lookupMinions
                          , lookupCard
                          , lookupCards
                          ) where

import Firestone.Types
import Firestone.Game
import Firestone.Player
import Firestone.IdGenerator

import Control.Lens
import Control.Monad.State

lookupMultiple :: (IdGenerator -> String -> (a, IdGenerator))
               -> IdGenerator -> [String] -> ([a], IdGenerator)
lookupMultiple lookup gen = foldr go ([], gen)
  where
    go name (rest, gen) = (lookup gen name) & _1 %~ (flip (:) rest)

lookupMinions :: IdGenerator -> [String] -> ([Minion], IdGenerator)
lookupMinions = lookupMultiple lookupMinion

lookupCards :: IdGenerator -> [String] -> ([Card], IdGenerator)
lookupCards = lookupMultiple lookupCard

lookupMinionM :: String -> State Game Minion
lookupMinionM name = do
    gen1 <- use idGen
    let (minion, gen2) = lookupMinion gen1 name
    idGen .= gen2
    return minion

lookupMinion :: IdGenerator -> String -> (Minion, IdGenerator)

lookupMinion gen name@"Oasis Snapjaw" = (minion, newGen)
  where
    (mId, mTime, newGen) = create gen name
    minion = makeMinion mId name 2 7 None [] True mTime []

lookupMinion gen name@"Murloc Raider" = (minion, newGen)
  where
    (mId, mTime, newGen) = create gen name
    minion = makeMinion mId name 2 1 Murloc [] True mTime []

lookupMinion gen name@"Magma Rager" = (minion, newGen)
  where
    (mId, mTime, newGen) = create gen name
    minion = makeMinion mId name 5 1 None [] True mTime []

lookupMinion gen name@"Imp Gang Boss" = (minion, newGen)
  where
    (mId, mTime, newGen) = create gen name
    minion = makeMinion mId name 2 4 Demon [] True mTime [Trigger MinionDamaged summonImp]

    summonImp :: Bool -> MinionLens -> State Game ()
    summonImp isMe m = case isMe of
        True  -> do
            imp <- lookupMinionM "Imp"
            me <- prerror m "Invalid minion sent to summonImp"
            position <- positionOf me
            zoom (ownerOf me) $ summonMinionAt (position + 1) imp
        False -> do
            return ()

lookupMinion gen name@"Imp" = (minion, newGen)
  where
    (mId, mTime, newGen) = create gen name
    minion = makeMinion mId name 1 1 Demon [] True mTime []

lookupMinion gen name@"Murloc Tinyfin" = (minion, newGen)
  where
    (mId, mTime, newGen) = create gen name
    minion = makeMinion mId name 1 1 Murloc [] True mTime []

lookupCard :: IdGenerator -> String -> (Card, IdGenerator)

lookupCard gen name@"Oasis Snapjaw" = (card, newGen)
  where
    (cId, _, newGen) = create gen name
    card = makeCard cId name 4 (Just 2) (Just 7) MinionCard "" False

lookupCard gen name@"Murloc Raider" = (card, newGen)
  where
    (cId, _, newGen) = create gen name
    card = makeCard cId name 1 (Just 2) (Just 1) MinionCard "" False

lookupCard gen name@"Magma Rager" = (card, newGen)
  where
    (cId, _, newGen) = create gen name
    card = makeCard cId name 3 (Just 5) (Just 1) MinionCard "" False

lookupCard gen name@"Imp Gang Boss" = (card, newGen)
  where
    (cId, _, newGen) = create gen name
    card = makeCard cId name 3 (Just 2) (Just 4) MinionCard "" False

lookupCard gen name@"Imp" = (card, newGen)
  where
    (cId, _, newGen) = create gen name
    card = makeCard cId name 1 (Just 1) (Just 1) MinionCard "" False

lookupCard gen name@"Murloc Tinyfin" = (card, newGen)
  where
    (cId, _, newGen) = create gen name
    card = makeCard cId name 0 (Just 1) (Just 1) MinionCard "" False