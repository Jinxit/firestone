{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Firestone.Card where

import Control.Lens

data Type = Minion
          | Spell
          | Weapon
          deriving (Show, Eq)

data Card = Card { _cardId :: String
                 , _cardName :: String
                 , _cardManaCost :: Int
                 , _cardOriginalManaCost :: Int
                 , _cardAttack :: Maybe Int
                 , _cardType :: Type
                 , _cardDescription :: String
                 , _cardIsTargeting :: Bool
                 } deriving (Show)

makeLenses ''Card

instance Eq Card where
    (==) a b = a^.cardId  == b^.cardId 

instance Ord Card where
    (<)  a b = a^.cardId  <  b^.cardId 
    (<=) a b = a^.cardId  <= b^.cardId 
    (>)  a b = a^.cardId  >  b^.cardId 
    (>=) a b = a^.cardId  >= b^.cardId 