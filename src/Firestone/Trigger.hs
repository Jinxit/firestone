{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE LiberalTypeSynonyms    #-}
{-# LANGUAGE ImpredicativeTypes     #-}

module Firestone.Trigger ( Triggerable(..)
                         ) where

import Firestone.Types
import Firestone.Utils

import Control.Lens
import Control.Monad.State
import Data.List

class Triggerable c where
    trigger :: TriggerType -> ATraversal' Game c -> State Game [Event]

instance Triggerable Minion where
    trigger t aC = do
        let c = cloneTraversal aC
        mId <- prerror (c.uuid) "Invalid character sent to trigger"
        let isSame minion = minion^.uuid == mId
        p1Minions <- use (p1.activeMinions)
        p2Minions <- use (p2.activeMinions)
        let allMinions = sort (p1Minions ++ p2Minions)
        let allTriggers = map (\minion -> (isSame minion, minion^.triggers)) allMinions
        let matchingTriggers = map (\(same, triggers) -> (same, filter ((== t) . triggerType) triggers)) allTriggers
        let actions = concat $ map (\(same, triggers) -> map (\trig -> (triggerAction trig) same) triggers) matchingTriggers
        forM_ actions $ \action -> action (cloneTraversal aC)
        --let dumb = map (\(same, triggers) -> same) matchingTriggers
        --deb <- use debug
        --debug .= (sum $ map fromEnum dumb):deb
        return []

instance Triggerable Hero where
    trigger t aC = return []
