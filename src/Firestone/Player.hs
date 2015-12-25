{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Firestone.Player where

import Firestone.Hero
import Firestone.Card
import Firestone.Minion
import Firestone.Deck

import Control.Lens

data Player = Player { _playerId :: String
                     , _playerHero :: Hero
                     , _playerHand :: [Card]
                     , _playerActiveMinions :: [Minion]
                     , _playerDeck :: Deck
                     } deriving (Show)

makeLenses ''Player

instance Eq Player where
    (==) a b = a^.playerId  == b^.playerId

instance Ord Player where
    (<)  a b = a^.playerId  <  b^.playerId
    (<=) a b = a^.playerId  <= b^.playerId
    (>)  a b = a^.playerId  >  b^.playerId
    (>=) a b = a^.playerId  >= b^.playerId

makePlayer :: String -> Hero -> Player
makePlayer pId pHero = Player pId pHero [] [] (Deck [])
