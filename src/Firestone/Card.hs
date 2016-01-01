{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Firestone.Card ( Type(..)
                      , Card(..)
                      , HasUuid(..)
                      , HasName(..)
                      , CardLens(..)
                      , manaCost
                      , makeCard) where

import {-# SOURCE #-} Firestone.Game

import Control.Lens

data Type = MinionCard
          | SpellCard
          | WeaponCard
          deriving (Show, Eq)

data Card = Card { cardUuid :: String
                 , cardName :: String
                 , cardManaCost :: Int
                 , cardOriginalManaCost :: Int
                 , cardAttack :: Maybe Int
                 , cardHealth :: Maybe Int
                 , cardCardType :: Type
                 , cardDescription :: String
                 , cardIsTargeting :: Bool
                 } deriving (Show)

type CardLens = Traversal' Game Card

makeFields ''Card

instance Eq Card where
    (==) a b = a^.uuid == b^.uuid

makeCard :: String -> String -> Int -> Maybe Int -> Maybe Int -> Type -> String -> Bool -> Card
makeCard cId cName cMana cAttack cHealth cType cDesc cIsTargeting =
    Card cId cName cMana cMana cAttack cHealth cType cDesc cIsTargeting