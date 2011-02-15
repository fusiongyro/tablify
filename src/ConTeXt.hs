module ConTeXt (converter) where

import Prelude hiding (length, replicate)
import Utilities
import Converter

-- Some ConTeXt.
texify :: Table -> String
texify table = intercalate "\n" [tblstart, hl, body, hl, tblend, hl]
	where
		columns = length $ head table
		tblstart = "\\starttable[|" ++ heading ++ "|]"
		tblend = "\\stoptable"
		heading = intersperse '|' $ replicate columns 'l'
		hl = "\\HL[3]\n"
		body = intercalate "\\\\\n" $ map (intercalate " & ") table

converter :: Converter
converter = Converter "ConTeXt" texify "C" "context"
