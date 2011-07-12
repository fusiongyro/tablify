module Converter where

import Data.ByteString

import Utilities

data Converter = Converter
    { cName         :: String
    , cConvert      :: Table -> ByteString
    , cShortOpt     :: String
    , cLongOpt      :: String
    }
