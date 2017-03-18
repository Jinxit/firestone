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

interpret :: Script () -> State Game [Event]
interpret script = case script of 
    Free (DamageMinion minion amount next) -> do
        damage minion amount
        interpret next
    Free (SpawnMinion player name pos next) -> do
        index <- case pos of
                        (Index i) -> return i
                        Last -> return $ views activeMinions length
                        (Adjacent minion side) -> do
                            i2 <- positionOf minion
                            return (case side of
                                        Left -> i2
                                        Right -> i2 + 1)

        gen1 <- use idGen
        let (minion, gen2) = lookupMinion gen1 name
        idGen .= gen2
        zoom player $ summonMinionAt index minion
        interpret next 
    Free (GetMinions player next) -> do
        let minions = player^.activeMinions
        interpret (next minions)
    Pure _ -> return []
