{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE LiberalTypeSynonyms    #-}
{-# LANGUAGE ImpredicativeTypes     #-}

module Firestone.Utils where

import Control.Lens
import Control.Monad.State
import Data.Monoid

prerror :: MonadState s m => Getting (First a) s a -> String -> m a
prerror getter err = do
    maybeValue <- preuse getter
    case maybeValue of
        Just x  -> return x
        Nothing -> error err
