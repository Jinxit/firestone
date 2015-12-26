{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Firestone.Event where

import Firestone.Minion
import Firestone.Card

import Control.Lens

data Event = Attack   { eventAttacker :: Minion
                      , eventTarget :: Minion
                      }
           | PlayCard { eventCard :: Card
                      , eventTarget :: Minion
                      }

makeFields ''Event
