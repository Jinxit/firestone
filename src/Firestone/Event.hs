{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Firestone.Event where

import Firestone.Minion
import Firestone.Card

import Control.Lens

data Event = Attack   { _eventAttacker :: Minion
                      , _eventTarget :: Minion
                      }
           | PlayCard { _eventCard :: Card
                      , _eventTarget :: Minion
                      }

makeLenses ''Event