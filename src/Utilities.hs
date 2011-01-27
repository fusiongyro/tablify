{-# LANGUAGE RankNTypes #-}

module Utilities
     (Table,
     module Data.List,
     repeats,
     length,
     replicate
    ) where

import Data.List hiding (length, replicate)
import Prelude hiding (length, replicate)

type Table = [[String]]

-- let's simplify the reading of the code
length :: forall b. [b] -> Integer
length = genericLength

replicate :: forall a. Integer -> a -> [a]
replicate = genericReplicate

repeats :: [a] -> Integer -> [a]
repeats s c = concat $ genericTake c $ repeat s
