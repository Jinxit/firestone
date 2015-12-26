{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Firestone.Player where

import Firestone.Hero
import Firestone.Card
import Firestone.Minion
import Firestone.Deck

import Control.Monad.State
import Control.Lens

data Player = Player { _playerId :: String
                     , _playerHero :: Hero
                     , _playerHand :: [Card]
                     , _playerActiveMinions :: [Minion]
                     , _playerDeck :: Deck
                     , _playerFatigue :: Int
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
makePlayer pId pHero = Player pId pHero [] [] (Deck []) 1

drawCard :: State Player ()
drawCard = do
    player <- get
    let cards = player^.playerDeck.deckCards
    case length cards of
        0 -> do
            playerHero.heroHealth -= player^.playerFatigue
            playerFatigue += 1
        otherwise -> do
            let (x:xs) = player^.playerDeck.deckCards
            playerHand %= take 10 . (|> x)
            playerDeck.deckCards .= xs
