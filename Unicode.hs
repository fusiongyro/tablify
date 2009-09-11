module Unicode (converter) where

import Utilities
import Converter
import FixedWidth

-- our basic agenda here is to convert the input into an array then we're
-- going to process the array to discover the maximum widths for each column.
-- once we have these maximums, we'll begin constructing the output by
-- prefixing and suffixing padded strings in the tabular format.
-- we take the first row to be the header. empty cells are permitted.

unicate :: Table -> String
unicate = fixedWidthTable ("┌","┬","┐") ("├","┼","┤") ("└","┴","┘") '─' '│'

converter = Converter "Unicode" unicate "U" "unicode"