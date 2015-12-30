module GameSpec where

import Firestone.Game
import Firestone.Player
import Firestone.GameBuilder
import Firestone.Card
import Firestone.Hero hiding (canAttack)
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
            length (game^.players.ix 0.hand) `shouldBe` 4
            length (game^.players.ix 1.hand) `shouldBe` 4
            length (game^.players.ix 0.deck.cards) `shouldBe` 2
            length (game^.players.ix 1.deck.cards) `shouldBe` 2
            let game2 = play game $ replicateM 4 endTurn
            length (game2^.players.ix 0.hand) `shouldBe` 6
            length (game2^.players.ix 1.hand) `shouldBe` 6
            length (game2^.players.ix 0.deck.cards) `shouldBe` 0
            length (game2^.players.ix 1.deck.cards) `shouldBe` 0

        it "should damage heroes when drawing from empty deck" $ do
            let game = buildGame $ addPlayers 2
            length (game^.players.ix 0.deck.cards) `shouldBe` 0
            length (game^.players.ix 1.deck.cards) `shouldBe` 0
            game^?!players.ix 0.hero.health `shouldBe` 20
            game^?!players.ix 1.hero.health `shouldBe` 20
            let game2 = play game $ replicateM 6 endTurn
            game2^?!players.ix 0.hero.health `shouldBe` 2
            game2^?!players.ix 1.hero.health `shouldBe` 2

    describe "mana" $ do
        it "should start with correct mana" $ do
            let game = buildGame $ addPlayers 2
            game^?!players.ix 0.hero.mana `shouldBe` 1
            game^?!players.ix 0.hero.maxMana `shouldBe` 1
            game^?!players.ix 1.hero.mana `shouldBe` 0
            game^?!players.ix 1.hero.maxMana `shouldBe` 0

    describe "attack" $ do
        it "should be able to attack hero with minions" $ do
            let game = buildGame $ do
                    addPlayers 2
                    setActiveMinions 1 ["Murloc Raider"]
            let p1 = game^?!players.ix 0
            let p2 = game^?!players.ix 1
            p1^.hero.health `shouldBe` 20
            let game2 = play game $ attack p1 (p1^?!activeMinions.ix 0.uuid) (p2^.hero.uuid)
            game2^?!players.ix 1.hero.health `shouldBe` 18

        it "can only attack on player's turn" $ do
            let game = buildGame $ do
                    addPlayers 2
                    setActiveMinions 1 ["Murloc Raider"]
                    setActiveMinions 2 ["Murloc Raider"]
            let p1 = game^?!players.ix 0
            let p2 = game^?!players.ix 1
            let p1m = p1^?!activeMinions.ix 0
            let p2m = p2^?!activeMinions.ix 0
            evalState (canAttack p1 p1m) game `shouldBe` True
            evalState (canAttack p2 p2m) game `shouldBe` False
            let game2 = play game endTurn
            let p1' = game2^?!players.ix 0
            let p2' = game2^?!players.ix 1
            let p1m' = p1'^?!activeMinions.ix 0
            let p2m' = p2'^?!activeMinions.ix 0
            evalState (canAttack p1' p1m') game2 `shouldBe` False
            evalState (canAttack p2' p2m') game2 `shouldBe` True
