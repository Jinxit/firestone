{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Firestone.Hero ( Hero(..)
                      , makeHero
                      , health
                      , maxHealth
                      , mana
                      , maxMana
                      , increaseMana
                      ) where

import Control.Monad.State
import Control.Lens

data Hero = Hero { heroUuid :: String
                 , heroName :: String
                 , heroHealth :: Int
                 , heroMaxHealth :: Int
                 , heroMana :: Int
                 , heroMaxMana :: Int
                 } deriving (Show)

makeFields ''Hero

instance Eq Hero where
    (==) a b = a^.uuid  == b^.uuid

instance Ord Hero where
    (<)  a b = a^.uuid  <  b^.uuid
    (<=) a b = a^.uuid  <= b^.uuid
    (>)  a b = a^.uuid  >  b^.uuid
    (>=) a b = a^.uuid  >= b^.uuid

makeHero :: String -> String -> Int -> Int -> Hero
makeHero hId hName hHp hMana = Hero hId hName hHp hHp hMana hMana

increaseMana :: State Hero ()
increaseMana = do
    maxMana %= min 10 . (+ 1)
    mana <~ use maxMana
