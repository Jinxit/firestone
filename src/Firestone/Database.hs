module Firestone.Database where

import Firestone.Minion
import Firestone.IdGenerator

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
