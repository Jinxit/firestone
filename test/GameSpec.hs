{-# LANGUAGE RankNTypes             #-}

module GameSpec where

import Firestone.Game
import Firestone.Player
import Firestone.GameBuilder
import Firestone.Card
import Firestone.Hero hiding (canAttack)
import Firestone.Minion hiding (canAttack)
import Firestone.Deck

import Control.Monad.State
import Control.Lens

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "draw cards" $ do
        it "should draw cards at start of game and turns" $ do
            let g = buildGame $ do
                    addPlayers 2
                    setDeck 1 (replicate 6 "Murloc Raider")
                    setDeck 2 (replicate 6 "Murloc Raider")
            length (g^?!p1.hand) `shouldBe` 4
            length (g^?!p2.hand) `shouldBe` 4
            length (g^?!p1.deck.cards) `shouldBe` 2
            length (g^?!p2.deck.cards) `shouldBe` 2
            let g2 = play g $ replicateM 4 endTurn
            length (g2^?!p1.hand) `shouldBe` 6
            length (g2^?!p2.hand) `shouldBe` 6
            length (g2^?!p1.deck.cards) `shouldBe` 0
            length (g2^?!p2.deck.cards) `shouldBe` 0

        it "should damage heroes when drawing from empty deck" $ do
            let g = buildGame $ addPlayers 2
            length (g^?!p1.deck.cards) `shouldBe` 0
            length (g^?!p2.deck.cards) `shouldBe` 0
            g^?!p1.hero.health `shouldBe` 20
            g^?!p2.hero.health `shouldBe` 20
            let g2 = play g $ replicateM 6 endTurn
            g2^?!p1.hero.health `shouldBe` 2
            g2^?!p2.hero.health `shouldBe` 2

    describe "mana" $ do
        it "should start with correct mana" $ do
            let g = buildGame $ addPlayers 2
            g^?!p1.hero.mana `shouldBe` 1
            g^?!p1.hero.maxMana `shouldBe` 1
            g^?!p2.hero.mana `shouldBe` 0
            g^?!p2.hero.maxMana `shouldBe` 0

    describe "attack" $ do
        it "should be able to attack hero with minions" $ do
            let g = buildGame $ do
                    addPlayers 2
                    setActiveMinions 1 ["Murloc Raider"]
            g^?!p1.hero.health `shouldBe` 20
            let g2 = play g $ attack (g^?!p1) (g^?!p1.m 0.uuid) (g^?!p2.hero.uuid)
            g2^?!p2.hero.health `shouldBe` 18

        it "can only attack on player's turn" $ do
            let g = buildGame $ do
                    addPlayers 2
                    setActiveMinions 1 ["Murloc Raider"]
                    setActiveMinions 2 ["Murloc Raider"]
            evalState (canAttack (g^?!p1) (g^?!p1.m 0)) g `shouldBe` True
            evalState (canAttack (g^?!p2) (g^?!p2.m 0)) g `shouldBe` False
            let g2 = play g endTurn
            evalState (canAttack (g2^?!p1) (g2^?!p1.m 0)) g2 `shouldBe` False
            evalState (canAttack (g2^?!p2) (g2^?!p2.m 0)) g2 `shouldBe` True

        it "can only attack once per minion per turn" $ do
            let g = buildGame $ do
                    addPlayers 2
                    setActiveMinions 1 ["Murloc Raider", "Murloc Raider"]
                    setActiveMinions 2 ["Murloc Raider", "Murloc Raider"]
            evalState (canAttack (g^?!p1) (g^?!p1.m 0)) g `shouldBe` True
            evalState (canAttack (g^?!p1) (g^?!p1.m 1)) g `shouldBe` True
            evalState (canAttack (g^?!p2) (g^?!p2.m 0)) g `shouldBe` False
            evalState (canAttack (g^?!p2) (g^?!p2.m 1)) g `shouldBe` False
            let g2 = play g $ attack (g^?!p1) (g^?!p1.m 0.uuid) (g^?!p2.hero.uuid)
            evalState (canAttack (g2^?!p1) (g2^?!p1.m 0)) g2 `shouldBe` False
            evalState (canAttack (g2^?!p1) (g2^?!p1.m 1)) g2 `shouldBe` True
            evalState (canAttack (g2^?!p2) (g2^?!p2.m 0)) g2 `shouldBe` False
            evalState (canAttack (g2^?!p2) (g2^?!p2.m 1)) g2 `shouldBe` False
            let g3 = play g2 endTurn
            evalState (canAttack (g3^?!p1) (g3^?!p1.m 0)) g3 `shouldBe` False
            evalState (canAttack (g3^?!p1) (g3^?!p1.m 1)) g3 `shouldBe` False
            evalState (canAttack (g3^?!p2) (g3^?!p2.m 0)) g3 `shouldBe` True
            evalState (canAttack (g3^?!p2) (g3^?!p2.m 1)) g3 `shouldBe` True
            let g4 = play g3 $ attack (g3^?!p2) (g3^?!p2.m 0.uuid) (g3^?!p1.hero.uuid)
            evalState (canAttack (g4^?!p1) (g4^?!p1.m 0)) g3 `shouldBe` False
            evalState (canAttack (g4^?!p1) (g4^?!p1.m 1)) g3 `shouldBe` False
            evalState (canAttack (g4^?!p2) (g4^?!p2.m 0)) g3 `shouldBe` False
            evalState (canAttack (g4^?!p2) (g4^?!p2.m 1)) g3 `shouldBe` True

p1 :: Traversal' Game Player
p1 = players.ix 0

p2 :: Traversal' Game Player
p2 = players.ix 1

m :: Int -> Traversal' Player Minion
m i = activeMinions.ix i
