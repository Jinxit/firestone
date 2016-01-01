{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleContexts       #-}

module Firestone.Character where

import {-# SOURCE #-} Firestone.Game
import Firestone.Hero as H
import Firestone.Minion as M

import Control.Lens
import Control.Monad.State

class ( HasUuid c String
      , HasName c String
      , HasHealth c Int
      , HasMaxHealth c Int
      , HasAttackValue c Int
      , HasIsSleepy c Bool
      ) => IsCharacter c where
    canAttack :: c -> Bool

instance IsCharacter M.Minion where
    canAttack = M.canAttack
instance IsCharacter H.Hero where
    canAttack = H.canAttack

type CharacterLens a = Traversal' Game a

damage :: IsCharacter c => Int -> State c ()
damage d = health -= d
