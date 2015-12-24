module Firestone.Hero where

data Hero = Hero { heroId :: String
                 , heroName :: String
                 , heroHealth :: Int
                 , heroMaxHealth :: Int
                 , heroMana :: Int
                 , heroMaxMana :: Int
                 } deriving (Show)

instance Eq Hero where
    (==) a b = heroId a == heroId b

instance Ord Hero where
    (<)  a b = heroId a <  heroId b
    (<=) a b = heroId a <= heroId b
    (>)  a b = heroId a >  heroId b
    (>=) a b = heroId a >= heroId b

makeHero :: String -> String -> Int -> Int -> Hero
makeHero hId hName hHp hMana = Hero hId hName hHp hHp hMana hMana
