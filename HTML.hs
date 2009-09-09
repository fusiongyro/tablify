module HTML where
import Utilities
import Data.Array
import Data.List

-- yeah, I'm not sure how this happened either
htmlify :: Table -> String
htmlify table = thead (head tlist) ++ tbody (tail tlist) ++ tfoot
	where
		thead row  = "<table>\n  <thead>\n    <tr>" ++
					 concatMap (\x -> "\n      <th>" ++ x ++ "</th>") row ++ 
					 "\n    </tr>\n  </thead>"
		tbody rows = "\n  <tbody>" ++ concatMap trow rows ++ "\n  </tbody>"
		trow  row  = "\n    <tr>" ++ concatMap (\x -> "\n      <td>" ++ x ++ "</td>") row ++ "\n    </tr>"
		tfoot      = "\n</table>"
		tlist      = tableToList table
		
		tableToList :: Table -> [[String]]
		tableToList table = [ map (table!) $ range ((row,0), (row,columns)) | row <- [0..rows]]
		((0,0), (rows, columns)) = bounds table	
