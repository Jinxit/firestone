{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Firestone.Player ( drawCard
                        , summonMinionAt
                        ) where

import Firestone.Types
import Firestone.Utils

import Control.Monad.State
import Control.Lens

drawCard :: State Player ()
drawCard = do
    player <- get
    case length (player^.deck.cards) of
        0 -> do
            hero.health -= player^.fatigue
            fatigue += 1
        otherwise -> do
            let (x:xs) = player^.deck.cards
            hand %= take 10 . (|> x)
            deck.cards .= xs

summonMinionAt :: Int -> Minion -> State Player ()
summonMinionAt pos m = activeMinions %= insertAt pos m
