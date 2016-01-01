{-# LANGUAGE RankNTypes             #-}

module Firestone.Trigger where

import {-# SOURCE #-} Firestone.Game
import Firestone.Character
import Firestone.Minion

import Control.Monad.State

data Trigger = MinionDamaged (CharacterLens Minion -> State Game ())

instance Show Trigger where
  show (MinionDamaged f) = "MinionDamaged"
