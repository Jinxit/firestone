{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Firestone.Hero where

import Control.Lens

data Hero = Hero { _heroId :: String
                 , _heroName :: String
                 , _heroHealth :: Int
                 , _heroMaxHealth :: Int
                 , _heroMana :: Int
                 , _heroMaxMana :: Int
                 } deriving (Show)

makeLenses ''Hero

instance Eq Hero where
    (==) a b = a^.heroId  == b^.heroId

instance Ord Hero where
    (<)  a b = a^.heroId  <  b^.heroId
    (<=) a b = a^.heroId  <= b^.heroId
    (>)  a b = a^.heroId  >  b^.heroId
    (>=) a b = a^.heroId  >= b^.heroId

makeHero :: String -> String -> Int -> Int -> Hero
makeHero hId hName hHp hMana = Hero hId hName hHp hHp hMana hMana
