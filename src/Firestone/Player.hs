module Firestone.Player where

import Firestone.Hero
import Firestone.Card
import Firestone.Minion
import Firestone.Deck

data Player = Player { playerId :: String
                     , playerHero :: Hero
                     , playerHand :: [Card]
                     , playerActiveMinions :: [Minion]
                     , playerDeck :: Deck
                     } deriving (Show)

instance Eq Player where
    (==) a b = playerId a == playerId b

instance Ord Player where
    (<)  a b = playerId a <  playerId b
    (<=) a b = playerId a <= playerId b
    (>)  a b = playerId a >  playerId b
    (>=) a b = playerId a >= playerId b

makePlayer :: String -> Hero -> Player
makePlayer pId pHero = Player pId pHero [] [] (Deck [])
