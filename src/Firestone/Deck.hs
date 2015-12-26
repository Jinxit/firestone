{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Firestone.Deck ( Deck(..)
                      , cards
                      ) where

import Firestone.Card

import Control.Lens

data Deck = Deck { deckCards :: [Card]
                 } deriving (Show)

makeFields ''Deck
