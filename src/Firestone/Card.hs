module Firestone.Card where

data Type = Minion
          | Spell
          | Weapon
          deriving (Show, Eq)

data Card = Card { cardId :: String
                 , cardName :: String
                 , cardManaCost :: Int
                 , cardOriginalManaCost :: Int
                 , cardAttack :: Maybe Int
                 , cardType :: Type
                 , cardDescription :: String
                 , cardIsTargeting :: Bool
                 } deriving (Show)

instance Eq Card where
    (==) a b = cardId a == cardId b

instance Ord Card where
    (<)  a b = cardId a <  cardId b
    (<=) a b = cardId a <= cardId b
    (>)  a b = cardId a >  cardId b
    (>=) a b = cardId a >= cardId b
