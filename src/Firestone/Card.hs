{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Firestone.Card where

import Control.Lens

data Type = MinionCard
          | SpellCard
          | WeaponCard
          deriving (Show, Eq)

data Card = Card { _cardId :: String
                 , _cardName :: String
                 , _cardManaCost :: Int
                 , _cardOriginalManaCost :: Int
                 , _cardAttack :: Maybe Int
                 , _cardHealth :: Maybe Int
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

makeCard :: String -> String -> Int -> Maybe Int -> Maybe Int -> Type -> String -> Bool -> Card
makeCard cId cName cMana cAttack cHealth cType cDesc cIsTargeting =
    Card cId cName cMana cMana cAttack cHealth cType cDesc cIsTargeting