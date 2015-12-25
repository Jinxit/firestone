{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Firestone.Deck where

import Firestone.Card

import Control.Lens

data Deck = Deck { _deckCards :: [Card]
                 } deriving (Show)

makeLenses ''Deck