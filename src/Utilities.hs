{-# LANGUAGE RankNTypes #-}

module Utilities ( Table
                 , module T
                 , T.Text
                 , (++)
                 , T.intercalate
                 , T.intersperse
                 , T.replicate
                 ) where

import Prelude hiding ((++))
import qualified Data.Text as T

type Table = [[T.Text]]

(++) = T.append
