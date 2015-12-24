module Firestone.Deck where

import Firestone.Card

data Deck = Deck { deckCards :: [Card]
                 } deriving (Show)
