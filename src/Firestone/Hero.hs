{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Firestone.Hero ( increaseMana
                      ) where

import Firestone.Types

import Control.Monad.State
import Control.Lens

increaseMana :: State Hero ()
increaseMana = do
    maxMana %= min 10 . (+ 1)
    mana <~ use maxMana
