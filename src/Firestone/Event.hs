module Firestone.Event where

import Firestone.Minion
import Firestone.Card

data Event = Attack   { eventAttacker :: Minion
                      , eventTarget :: Minion
                      }
           | PlayCard { eventCard :: Card
                      , eventTarget :: Minion
                      }
