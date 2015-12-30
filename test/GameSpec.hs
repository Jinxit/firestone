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
            let game = buildGame $ do
                    addPlayers 2
                    setDeck 1 (replicate 6 "Murloc Raider")
                    setDeck 2 (replicate 6 "Murloc Raider")
            length (game^?!p1.hand) `shouldBe` 4
            length (game^?!p2.hand) `shouldBe` 4
            length (game^?!p1.deck.cards) `shouldBe` 2
            length (game^?!p2.deck.cards) `shouldBe` 2
            let game2 = play game $ replicateM 4 endTurn
            length (game2^?!p1.hand) `shouldBe` 6
            length (game2^?!p2.hand) `shouldBe` 6
            length (game2^?!p1.deck.cards) `shouldBe` 0
            length (game2^?!p2.deck.cards) `shouldBe` 0

        it "should damage heroes when drawing from empty deck" $ do
            let game = buildGame $ addPlayers 2
            length (game^?!p1.deck.cards) `shouldBe` 0
            length (game^?!p2.deck.cards) `shouldBe` 0
            game^?!p1.hero.health `shouldBe` 20
            game^?!p2.hero.health `shouldBe` 20
            let game2 = play game $ replicateM 6 endTurn
            game2^?!p1.hero.health `shouldBe` 2
            game2^?!p2.hero.health `shouldBe` 2

    describe "mana" $ do
        it "should start with correct mana" $ do
            let game = buildGame $ addPlayers 2
            game^?!p1.hero.mana `shouldBe` 1
            game^?!p1.hero.maxMana `shouldBe` 1
            game^?!p2.hero.mana `shouldBe` 0
            game^?!p2.hero.maxMana `shouldBe` 0

    describe "attack" $ do
        it "should be able to attack hero with minions" $ do
            let game = buildGame $ do
                    addPlayers 2
                    setActiveMinions 1 ["Murloc Raider"]
            game^?!p1.hero.health `shouldBe` 20
            let game2 = play game $ attack (game^?!p1) (game^?!p1.m 0.uuid) (game^?!p2.hero.uuid)
            game2^?!p2.hero.health `shouldBe` 18

        it "can only attack on player's turn" $ do
            let game = buildGame $ do
                    addPlayers 2
                    setActiveMinions 1 ["Murloc Raider"]
                    setActiveMinions 2 ["Murloc Raider"]
            evalState (canAttack (game^?!p1) (game^?!p1.m 0)) game `shouldBe` True
            evalState (canAttack (game^?!p2) (game^?!p2.m 0)) game `shouldBe` False
            let game2 = play game endTurn
            evalState (canAttack (game2^?!p1) (game2^?!p1.m 0)) game2 `shouldBe` False
            evalState (canAttack (game2^?!p2) (game2^?!p2.m 0)) game2 `shouldBe` True

        it "can only attack once per minion per turn" $ do
            let game = buildGame $ do
                    addPlayers 2
                    setActiveMinions 1 ["Murloc Raider", "Murloc Raider"]
                    setActiveMinions 2 ["Murloc Raider", "Murloc Raider"]
            evalState (canAttack (game^?!p1) (game^?!p1.m 0)) game `shouldBe` True
            evalState (canAttack (game^?!p1) (game^?!p1.m 1)) game `shouldBe` True
            evalState (canAttack (game^?!p2) (game^?!p2.m 0)) game `shouldBe` False
            evalState (canAttack (game^?!p2) (game^?!p2.m 1)) game `shouldBe` False
            let game2 = play game $ attack (game^?!p1) (game^?!p1.m 0.uuid) (game^?!p2.hero.uuid)
            evalState (canAttack (game2^?!p1) (game2^?!p1.m 0)) game2 `shouldBe` False
            evalState (canAttack (game2^?!p1) (game2^?!p1.m 1)) game2 `shouldBe` True
            evalState (canAttack (game2^?!p2) (game2^?!p2.m 0)) game2 `shouldBe` False
            evalState (canAttack (game2^?!p2) (game2^?!p2.m 1)) game2 `shouldBe` False
            let game3 = play game endTurn
            evalState (canAttack (game3^?!p1) (game3^?!p1.m 0)) game3 `shouldBe` False
            evalState (canAttack (game3^?!p1) (game3^?!p1.m 1)) game3 `shouldBe` False
            evalState (canAttack (game3^?!p2) (game3^?!p2.m 0)) game3 `shouldBe` True
            evalState (canAttack (game3^?!p2) (game3^?!p2.m 1)) game3 `shouldBe` True
            let game4 = play game $ attack (game^?!p2) (game^?!p2.m 0.uuid) (game^?!p1.hero.uuid)
            evalState (canAttack (game3^?!p1) (game3^?!p1.m 0)) game3 `shouldBe` False
            evalState (canAttack (game3^?!p1) (game3^?!p1.m 1)) game3 `shouldBe` False
            evalState (canAttack (game3^?!p2) (game3^?!p2.m 0)) game3 `shouldBe` False
            evalState (canAttack (game3^?!p2) (game3^?!p2.m 1)) game3 `shouldBe` True

p1 :: Traversal' Game Player
p1 = players.ix 0

p2 :: Traversal' Game Player
p2 = players.ix 1

m :: Int -> Traversal' Player Minion
m i = activeMinions.ix i
