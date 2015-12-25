module Firestone.IdGenerator where

data IdGenerator = IdGenerator Int
                 deriving (Show)

makeIdGenerator :: IdGenerator
makeIdGenerator = IdGenerator 0

create :: IdGenerator -> String -> (String, IdGenerator)
create (IdGenerator n) name = (name ++ (show n), IdGenerator (n + 1))
