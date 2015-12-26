{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Firestone.Player ( Player(..)
                        , hero
                        , activeMinions
                        , deck
                        , makePlayer
                        , drawCard
                        ) where

import Firestone.Hero
import Firestone.Card
import Firestone.Minion
import Firestone.Deck

import Control.Monad.State
import Control.Lens

data Player = Player { playerUuid :: String
                     , playerHero :: Hero
                     , playerHand :: [Card]
                     , playerActiveMinions :: [Minion]
                     , playerDeck :: Deck
                     , playerFatigue :: Int
                     } deriving (Show)

makeFields ''Player

instance Eq Player where
    (==) a b = a^.uuid  == b^.uuid

instance Ord Player where
    (<)  a b = a^.uuid  <  b^.uuid
    (<=) a b = a^.uuid  <= b^.uuid
    (>)  a b = a^.uuid  >  b^.uuid
    (>=) a b = a^.uuid  >= b^.uuid

makePlayer :: String -> Hero -> Player
makePlayer pId pHero = Player pId pHero [] [] (Deck []) 1

drawCard :: State Player ()
drawCard = do
    player <- get
    let deckCards = player^.deck.cards
    case length deckCards of
        0 -> do
            hero.health -= player^.fatigue
            fatigue += 1
        otherwise -> do
            let (x:xs) = deckCards
            hand %= take 10 . (|> x)
            deck.cards .= xs
