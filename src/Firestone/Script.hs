module Firestone.Script where

import Firestone.Types
import Firestone.Game
import Firestone.Utils

import Control.Lens
import Control.Monad.State
import Control.Monad.Free


damageMinion :: MinionLens -> Script ()
damageMinion minion amount = liftF (DamageMinion minion amount ())

spawnMinion :: PlayerLens -> String -> Position -> Script ()
spawnMinion player name pos = liftF (SpawnMinion player name pos ())

getMinions :: PlayerLens -> Script MinionLens
getMinions player = liftF (GetMinions player id)
