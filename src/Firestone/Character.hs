{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleContexts       #-}

module Firestone.Character ( damage
                           ) where

import Firestone.Types

import Control.Lens
import Control.Monad.State

damage :: (IsCharacter c, Triggerable c) => CharacterLens c -> Int -> State Game [Event]
damage c d = do
    c.health -= d
    char <- use c
    case d > 0 of
        True  -> trigger MinionDamaged char
        False -> return []
    return []
