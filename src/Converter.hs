{-# LANGUAGE UnicodeSyntax #-}
module Converter where

import Data.ByteString

import Utilities

data Converter = Converter
    { cName         :: String
    , cConvert      :: Table -> Text
    , cShortOpt     :: String
    , cLongOpt      :: String
    }
