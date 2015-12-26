module Firestone.Error where

import Control.Monad.Except

data FirestoneError = IllegalAction String
                    | MiscError String

instance Show FirestoneError where
    show (IllegalAction str) = "Illegal action: " ++ str
