{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleContexts       #-}

module Firestone.Character where

import Firestone.Hero as H
import Firestone.Minion as M

import Control.Lens
import Control.Monad.State

data Character = CMinion M.Minion
               | CHero Hero

instance HasUuid Character String where
    uuid f (CMinion m) = CMinion <$> uuid f m
    uuid f (CHero h) = CHero <$> uuid f h

instance HasName Character String where
    name f (CMinion m) = CMinion <$> name f m
    name f (CHero h) = CHero <$> name f h

instance HasHealth Character Int where
    health f (CMinion m) = CMinion <$> health f m
    health f (CHero h) = CHero <$> health f h

instance HasMaxHealth Character Int where
    maxHealth f (CMinion m) = CMinion <$> maxHealth f m
    maxHealth f (CHero h) = CHero <$> maxHealth f h

instance HasAttackValue Character Int where
    attackValue f (CMinion m) = CMinion <$> attackValue f m
    attackValue f (CHero h) = CHero <$> attackValue f h

instance Eq Character where
    (==) a b = a^.uuid == b^.uuid

class ( HasUuid c String
      , HasName c String
      , HasHealth c Int
      , HasMaxHealth c Int
      , HasAttackValue c Int
      ) => IsCharacter c where
    canAttack :: c -> Bool

instance IsCharacter M.Minion where
    canAttack = M.canAttack
instance IsCharacter H.Hero where
    canAttack = H.canAttack

damage :: IsCharacter c => Int -> State c ()
damage d = health -= d
