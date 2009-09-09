module HTML where

import Data.Array
import Data.List
import Text.Regex
import Utilities

data Tag = Tag { tagName :: String, tagBody :: Tag } 
		 | TList [Tag]
		 | Body String
		deriving Show

indent :: Integer -> String
indent n = genericReplicate (n*2) ' '

start :: String -> String
start s = "<" ++ s ++ ">"

end :: String -> String
end s = "</" ++ s ++ ">"

htmlEscape :: String -> String
htmlEscape html = foldl' process html mappings
	where
		mappings = [("&", "&amp;"), ("<", "&lt;"), (">", "&gt;")]
		process text (find, replace) = subRegex (mkRegex find) text replace

renderTag :: Integer -> Tag -> String
renderTag l (Tag name (Body bod)) =
	indent l ++ start name ++ htmlEscape bod ++ end name ++ "\n"
renderTag l (Tag name (TList tags)) =
	   indent l ++ start name ++ "\n" 
	++ (concatMap (renderTag (succ l)) tags) 
	++ indent l ++ end name ++ "\n"
renderTag l (Tag name bod) =
	   indent l 
	++ start name ++ "\n" 
	++ renderTag (succ l) bod 
	++ indent l ++ end name ++ "\n"
renderTag level (Body bod) = indent level ++ htmlEscape bod

render :: Tag -> String
render = renderTag 0

htmlify :: Table -> String
htmlify table = render tableToTags
	where
		tableToTags = Tag "table" (TList [header, body])
				
		header, body :: Tag
		header = Tag "thead" $ rowToTags "th" (row table 0)
		body   = Tag "tbody" $ TList $ 
					map (rowToTags "td") [ row table n | n <- [1..rows]]
		
		rowToTags :: String -> [String] -> Tag
		rowToTags cellT row = Tag "tr" (TList $ map (cellToTag cellT) row)
		
		cellToTag :: String -> String -> Tag
		cellToTag cellT cell = Tag cellT (Body cell)
		
		((0,0), (rows,columns)) = bounds table