{-# LANGUAGE RankNTypes             #-}

module Firestone.Database ( lookupMinion
                          , lookupMinions
                          , lookupCard
                          , lookupCards
                          ) where

import Firestone.Types
import Firestone.IdGenerator

lookupMultiple :: (IdGenerator -> String -> (a, IdGenerator))
               -> IdGenerator -> [String] -> ([a], IdGenerator)
lookupMinions :: IdGenerator -> [String] -> ([Minion], IdGenerator)
lookupCards :: IdGenerator -> [String] -> ([Card], IdGenerator)
lookupMinion :: IdGenerator -> String -> (Minion, IdGenerator)
lookupCard :: IdGenerator -> String -> (Card, IdGenerator)
