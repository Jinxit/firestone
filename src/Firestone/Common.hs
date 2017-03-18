{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE LiberalTypeSynonyms    #-}

module Firestone.Common where

import Firestone.Types

import Control.Lens
import Control.Monad.State
import Control.Monad.Free
import Data.Maybe
import Data.List

ownerOf :: HasUuid a String => a -> PlayerLens
ownerOf u f g =
    case ownsAny p1 of
        True  -> p1 f g
        False -> case ownsAny p2 of
                    True  -> p2 f g
                    False -> error ("Ownerless thing found: " ++ (u^.uuid))
  where
    ownsMinion p = any (\m -> m^.uuid == u^.uuid) (p^.activeMinions)
    ownsHero p = (u^.uuid) == (p^.hero^.uuid)
    ownsCard p = any (\c -> c^.uuid == u^.uuid) (p^.hand)
    ownsAny p = ownsMinion (g^.p)
             || ownsHero (g^.p)
             || ownsCard (g^.p)

positionOf :: Minion -> State Game Int
positionOf minion = do
    owner <- use (ownerOf minion)
    let pos = (minion `elemIndex` (owner^.activeMinions))
    return $ fromMaybe (-1) pos
