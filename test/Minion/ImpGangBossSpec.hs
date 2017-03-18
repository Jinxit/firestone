{-# LANGUAGE RankNTypes             #-}

module Minion.ImpGangBossSpec where

import Firestone.Game
import Firestone.Types
import Firestone.GameBuilder

import Control.Monad.State
import Control.Lens
import Control.Exception
import Data.Either

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "when taking damage" $ do
        it "should summon imp" $ do
            let g = buildGame $ do
                    setActiveMinions 1 (replicate 4 "Murloc Tinyfin")
                    setActiveMinions 2 ["Murloc Raider", "Imp Gang Boss"]
            let g2 = play g $ replicateM_ 2 $ attack (p1.m 0) (p2.m 1)
            length (g2^.p1.activeMinions) `shouldBe` 2
            length (g2^.p2.activeMinions) `shouldBe` 4
            g2^.p2.m 0.name `shouldBe` "Murloc Raider"
            g2^.p2.m 1.name `shouldBe` "Imp Gang Boss"
            g2^.p2.m 2.name `shouldBe` "Imp"
            g2^.p2.m 3.name `shouldBe` "Imp"
            let g3 = play g2 $ replicateM_ 2 $ attack (p1.m 0) (p2.m 1)
            length (g3^.p1.activeMinions) `shouldBe` 0
            length (g3^.p2.activeMinions) `shouldBe` 5
            g3^.p2.m 0.name `shouldBe` "Murloc Raider"
            g3^.p2.m 1.name `shouldBe` "Imp"
            g3^.p2.m 2.name `shouldBe` "Imp"
            g3^.p2.m 3.name `shouldBe` "Imp"
            g3^.p2.m 4.name `shouldBe` "Imp"
            let checkTimestamp lhs rhs = (g3^?!p2.m lhs.timestamp) `shouldSatisfy`
                                         (< (g3^?!p2.m rhs.timestamp))
            (g3^?!p2.m 1.timestamp) > (g3^?!p2.m 2.timestamp) `shouldBe` True
            (g3^?!p2.m 2.timestamp) > (g3^?!p2.m 3.timestamp) `shouldBe` True
            (g3^?!p2.m 3.timestamp) > (g3^?!p2.m 4.timestamp) `shouldBe` True
            (g3^?!p2.m 4.timestamp) > (g3^?!p2.m 0.timestamp) `shouldBe` True

m :: Int -> Lens' Player Minion
m i = unsafeSingular $ activeMinions.ix i
