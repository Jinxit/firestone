module Firestone.Database where

import Firestone.Minion
import Firestone.Card
import Firestone.IdGenerator

import Control.Lens

lookupMultiple :: (IdGenerator -> String -> (a, IdGenerator))
               -> IdGenerator -> [String] -> ([a], IdGenerator)
lookupMultiple lookup idGen = foldr go ([], idGen)
  where
    go name (rest, gen) = (lookup gen name) & _1 %~ (flip (:) rest)

lookupMinions :: IdGenerator -> [String] -> ([Minion], IdGenerator)
lookupMinions = lookupMultiple lookupMinion

lookupCards :: IdGenerator -> [String] -> ([Card], IdGenerator)
lookupCards = lookupMultiple lookupCard

-- todo: Imp Gang Boss

lookupMinion :: IdGenerator -> String -> (Minion, IdGenerator)

lookupMinion idGen name@"Oasis Snapjaw" = (minion, newGen)
  where
    (mId, newGen) = create idGen name
    minion = makeMinion mId name 2 7 None [] True

lookupMinion idGen name@"Murloc Raider" = (minion, newGen)
  where
    (mId, newGen) = create idGen name
    minion = makeMinion mId name 2 1 Murloc [] True

lookupMinion idGen name@"Magma Rager" = (minion, newGen)
  where
    (mId, newGen) = create idGen name
    minion = makeMinion mId name 5 1 None [] True

lookupMinion idGen name@"Imp" = (minion, newGen)
  where
    (mId, newGen) = create idGen name
    minion = makeMinion mId name 1 1 Demon [] True

lookupCard :: IdGenerator -> String -> (Card, IdGenerator)

lookupCard idGen name@"Oasis Snapjaw" = (card, newGen)
  where
    (cId, newGen) = create idGen name
    card = makeCard cId name 4 (Just 2) (Just 7) MinionCard "" False

lookupCard idGen name@"Murloc Raider" = (card, newGen)
  where
    (cId, newGen) = create idGen name
    card = makeCard cId name 1 (Just 2) (Just 1) MinionCard "" False

lookupCard idGen name@"Magma Rager" = (card, newGen)
  where
    (cId, newGen) = create idGen name
    card = makeCard cId name 3 (Just 5) (Just 1) MinionCard "" False

lookupCard idGen name@"Imp" = (card, newGen)
  where
    (cId, newGen) = create idGen name
    card = makeCard cId name 1 (Just 1) (Just 1) MinionCard "" False
