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

instance HasAttack Character Int where
    attack f (CMinion m) = CMinion <$> attack f m
    attack f (CHero h) = CHero <$> attack f h

class ( HasUuid s String
      , HasName s String
      , HasHealth s Int
      , HasMaxHealth s Int
      ) => IsCharacter s

instance IsCharacter M.Minion
instance IsCharacter H.Hero

damage :: IsCharacter a => Int -> State a ()
damage d = health -= d

canAttack :: Character -> Bool
canAttack (CMinion m) = M.canAttack m
canAttack (CHero h) = H.canAttack h
