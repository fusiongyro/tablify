{-# LANGUAGE OverloadedStrings #-}

module ASCII (converter) where
import Prelude hiding (length)

import Utilities
import Converter
import FixedWidth

-- our basic agenda here is to convert the input into an array then we're
-- going to process the array to discover the maximum widths for each column.
-- once we have these maximums, we'll begin constructing the output by
-- prefixing and suffixing padded strings in the tabular format.
-- we take the first row to be the header. empty cells are permitted.

asciify :: Table -> Text
asciify = fixedWidthTable ("+","+","+") ("+","+","+") ("+","+","+") '-' '|'

converter :: Converter
converter = Converter
    { cName        = "ASCII"
    , cConvert     = asciify
    , cShortOpt    = "A"
    , cLongOpt     = "ascii"
    }
