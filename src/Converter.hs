module Converter where

import Utilities

data Converter = Converter
    { cName         :: String
    , cConvert      :: Table -> String
    , cShortOpt     :: String
    , cLongOpt      :: String
    }
