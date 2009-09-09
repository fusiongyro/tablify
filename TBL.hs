module TBL where
import Data.Array
import Data.List

import Utilities

-- TBL... yeah...
tblify :: Table -> String
tblify table = tblstart ++ "\n" ++ header ++ "\n" ++ body ++ "\n" ++ tblend
	where
		((0,0), (rows, columns)) = bounds table
		tblstart = ".TS"
		tblend = ".TE"
		header = intercalate " " (genericReplicate (columns+1) "c") ++ "\n" ++ intercalate " " (genericReplicate (columns+1) "l") ++ "."
		body = intercalate "\n" $ [ formatRow n | n <- [0..rows]]
		formatRow n = intercalate "\t" $ rowValues n
		rowValues r        = [ x | x <- map (table!) $ range ((r,0), (r,columns)) ]		
