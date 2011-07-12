{-# LANGUAGE RankNTypes #-}

module Utilities
     (Table,
     module B,
     B.ByteString,
     (++)
    ) where

import Prelude hiding ((++))
import qualified Data.ByteString.Char8 as B

type Table = [[B.ByteString]]

(++) = B.append
