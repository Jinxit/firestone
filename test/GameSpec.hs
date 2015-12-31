{-# LANGUAGE RankNTypes             #-}

module GameSpec where

import Firestone.Game
import Firestone.Player
import Firestone.GameBuilder
import Firestone.Card
import Firestone.Hero hiding (canAttack)
import Firestone.Minion hiding (canAttack)
import Firestone.Deck
import Firestone.Event

import Control.Monad.State
import Control.Lens
import Data.Either

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "draw cards" $ do
        it "should draw cards at start of game and turns" $ do
            let g = buildGame $ do
                    setDeck 1 (replicate 6 "Murloc Raider")
                    setDeck 2 (replicate 6 "Murloc Raider")
            length (g^.p1.hand) `shouldBe` 4
            length (g^.p2.hand) `shouldBe` 4
            length (g^.p1.deck.cards) `shouldBe` 2
            length (g^.p2.deck.cards) `shouldBe` 2
            let g2 = play g $ replicateM 4 endTurn
            length (g2^.p1.hand) `shouldBe` 6
            length (g2^.p2.hand) `shouldBe` 6
            length (g2^.p1.deck.cards) `shouldBe` 0
            length (g2^.p2.deck.cards) `shouldBe` 0

        it "should damage heroes when drawing from empty deck" $ do
            let g = baseGame
            length (g^.p1.deck.cards) `shouldBe` 0
            length (g^.p2.deck.cards) `shouldBe` 0
            g^.p1.hero.health `shouldBe` 20
            g^.p2.hero.health `shouldBe` 20
            let g2 = play g $ replicateM 6 endTurn
            g2^.p1.hero.health `shouldBe` 2
            g2^.p2.hero.health `shouldBe` 2

    describe "mana" $ do
        it "should start with correct mana" $ do
            let g = baseGame
            g^.p1.hero.mana `shouldBe` 1
            g^.p1.hero.maxMana `shouldBe` 1
            g^.p2.hero.mana `shouldBe` 0
            g^.p2.hero.maxMana `shouldBe` 0
        it "should increase on new turn" $ do
            let g = baseGame
            let g2 = play g endTurn
            let checkManaAndEndTurn = do
                    (i, gn) <- get
                    let h1 = gn^.p1.hero
                    let h2 = gn^.p2.hero
                    liftIO $ h1^.mana `shouldBe` i
                    liftIO $ h1^.maxMana `shouldBe` i
                    liftIO $ h2^.mana `shouldBe` i
                    liftIO $ h2^.maxMana `shouldBe` i
                    put $ (i + 1, play gn $ replicateM 2 endTurn)
            (_, g3) <- execStateT (replicateM 10 checkManaAndEndTurn) (1, g2)
            g3^.p1.hero.mana `shouldBe` 10
            g3^.p1.hero.maxMana `shouldBe` 10
            g3^.p2.hero.mana `shouldBe` 10
            g3^.p2.hero.maxMana `shouldBe` 10
        it "should decrease when a card is played" $ do
            let g = buildGame $ do
                    setDeck 1 ["Murloc Raider", "Magma Rager"]
                    setStartingMana 1 10
            let g2 = play g playFirstMinionCard
            g2^.p1.hero.mana `shouldBe` 9
            let g3 = play g2 playFirstMinionCard
            g3^.p1.hero.mana `shouldBe` 6
            let g4 = play g3 $ replicateM 2 endTurn
            g4^.p1.hero.mana `shouldBe` 10

    describe "attack" $ do
        it "should be able to attack hero with minions" $ do
            let g = buildGame $ do
                    setActiveMinions 1 ["Murloc Raider"]
            g^.p1.hero.health `shouldBe` 20
            evalState (isAttackValid (p1.m 0) (p2.hero)) g `shouldBe` Right True
            let g2 = play g $ simpleAttackHero p1 p2 0
            g2^.p2.hero.health `shouldBe` 18

        it "can only attack on player's turn" $ do
            let g = buildGame $ do
                    setActiveMinions 1 ["Murloc Raider"]
                    setActiveMinions 2 ["Murloc Raider"]
            evalState (canAttack p1 (g^?!p1.m 0)) g `shouldBe` True
            evalState (canAttack p2 (g^?!p2.m 0)) g `shouldBe` False
            let g2 = play g endTurn
            evalState (canAttack p1 (g2^?!p1.m 0)) g2 `shouldBe` False
            evalState (canAttack p2 (g2^?!p2.m 0)) g2 `shouldBe` True

        it "can only attack once per minion per turn" $ do
            let g = buildGame $ do
                    setActiveMinions 1 ["Murloc Raider", "Murloc Raider"]
                    setActiveMinions 2 ["Murloc Raider", "Murloc Raider"]
            simpleCanAttack g p1 0 `shouldBe` True
            simpleCanAttack g p1 1 `shouldBe` True
            simpleCanAttack g p2 0 `shouldBe` False
            simpleCanAttack g p2 1 `shouldBe` False
            let g2 = play g $ simpleAttackHero p1 p2 0
            simpleCanAttack g2 p1 0 `shouldBe` False
            simpleCanAttack g2 p1 1 `shouldBe` True
            simpleCanAttack g2 p2 0 `shouldBe` False
            simpleCanAttack g2 p2 1 `shouldBe` False
            let g3 = play g2 endTurn
            simpleCanAttack g3 p1 0 `shouldBe` False
            simpleCanAttack g3 p1 1 `shouldBe` False
            simpleCanAttack g3 p2 0 `shouldBe` True
            simpleCanAttack g3 p2 1 `shouldBe` True
            let g4 = play g3 $ simpleAttackHero p2 p1 0
            simpleCanAttack g4 p1 0 `shouldBe` True
            simpleCanAttack g4 p1 1 `shouldBe` True
            simpleCanAttack g4 p2 0 `shouldBe` False
            simpleCanAttack g4 p2 1 `shouldBe` False

        it "should produce error when attacking twice per turn with same minion" $ do
            let g = buildGame $ do
                    setActiveMinions 1 ["Murloc Raider"]
            let g2 = play g (simpleAttackHero p1 p2 0)
            evalState (simpleAttackHero p1 p2 0) g2 `shouldSatisfy` isLeft

        it "can not attack with just played minions" $ do
            let g = buildGame $ do
                    setDeck 1 ["Murloc Raider"]
            let murlocCanAttack gn = evalState (canAttack p1 (gn^.p1^?!m 0)) gn
            let g2 = play g $ playFirstMinionCard
            murlocCanAttack g2 `shouldBe` False
            let g3 = play g2 $ replicateM_ 2 endTurn
            murlocCanAttack g3 `shouldBe` False

        it "can not attack friendly minions" $ do
            let g = buildGame $ do
                    setActiveMinions 1 ["Murloc Raider", "Magma Rager"]
            evalState (isAttackValid (p1.m 0) (p1.m 1)) g `shouldBe` Right False

        it "should remove dead minions" $ do
            let g = buildGame $ do
                    setActiveMinions 1 (replicate 4 "Murloc Raider")
                    setActiveMinions 2 ["Oasis Snapjaw"]
            let g2 = play g $ do
                    replicateM 3 $ simpleAttack p1 p2 0 0
                    endTurn
                    simpleAttack p2 p1 0 0
            length (g2^.p1.activeMinions) `shouldBe` 0
            length (g2^.p2.activeMinions) `shouldBe` 0

        it "shouldn't remove non-dead minions" $ do
            let g = buildGame $ do
                    setActiveMinions 1 ["Oasis Snapjaw", "Oasis Snapjaw"]
                    setActiveMinions 2 ["Oasis Snapjaw"]
            let g2 = play g $ do
                    simpleAttack p1 p2 0 0
                    simpleAttack p1 p2 1 0
                    endTurn
                    simpleAttack p2 p1 0 0
            length (g2^.p1.activeMinions) `shouldBe` 2
            length (g2^.p2.activeMinions) `shouldBe` 1
            g2^?!p1.m 0.health `shouldBe` 3
            g2^?!p1.m 1.health `shouldBe` 5
            g2^?!p2.m 0.health `shouldBe` 1

    describe "hero" $ do
        it "should end game when health <= 1" $ do
            let g = buildGame $ do
                    setActiveMinions 1 ["Magma Rager"]
                    setMaxHealth 2 11
            g^.active `shouldBe` True
            let g2 = play g $ simpleAttackHero p1 p2 0
            g2^.p2.hero.health `shouldBe` -4
            g2^.active `shouldBe` False

    describe "play cards" $ do
        it "should be able to summon minions" $ do
            let g = buildGame $ do
                    setDeck 1 ["Murloc Raider", "Magma Rager", "Murloc Raider"]
                    setStartingMana 1 10
            let g2 = play g $ replicateM_ 3 playFirstMinionCard
            length (g2^.p1.hand) `shouldBe` 0
            length (g2^.p1.activeMinions) `shouldBe` 0

m :: Int -> Traversal' Player Minion
m i = activeMinions.ix i

simpleAttack :: PlayerLens -> PlayerLens -> Int -> Int -> State Game (Either String [Event])
simpleAttack attacker target mi1 mi2 = do
    game <- get
    let attackerId = game^.attacker.m mi1.uuid
    let targetId = game^.target.m mi2.uuid
    attack attackerId targetId

simpleAttackHero :: PlayerLens -> PlayerLens -> Int -> State Game (Either String [Event])
simpleAttackHero attacker target mi = do
    game <- get
    let attackerId = game^.attacker.m mi.uuid
    let targetId = game^.target.hero.uuid
    attack attackerId targetId

simpleCanAttack :: Game -> PlayerLens -> Int -> Bool
simpleCanAttack game player mi = evalState (canAttack player (game^?!player.m mi)) game

playFirstMinionCard :: State Game (Either String [Event])
playFirstMinionCard = do
    p <- use playerInTurn
    playMinionCard (playerInTurn.hand.ix 0) (length (p^.activeMinions))
