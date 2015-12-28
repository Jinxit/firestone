module GameSpec where

import Firestone.Game
import Firestone.Player
import Firestone.GameBuilder
import Firestone.Card
import Firestone.Hero
import Firestone.Deck
import Firestone.Character

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
