{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE LiberalTypeSynonyms    #-}

module Firestone.Trigger ( Triggerable(..)
                         ) where

import Firestone.Types
import Firestone.Utils
import Firestone.Common

import Control.Lens
import Control.Monad.State
import Data.List
