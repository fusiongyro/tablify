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
length = genericLength
replicate = genericReplicate

repeats :: [a] -> Integer -> [a]
repeats s c = concat $ genericTake c $ repeat s
