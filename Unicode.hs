module Unicode where
import Data.Array
import Data.List

import Utilities

columnWidths :: Table -> [Integer]
columnWidths table = elems $ foldl' combine initial $ assocs table
	where
		((_,_), (rows, columns)) = bounds table
		initial = array (0,columns) [ (i, 0) | i <- [0..columns] ]
		combine tab ((_,y), val) = if genericLength val > tab!y then tab//[(y, genericLength val)] else tab

-- We used to call this ASCII art, but these days it's Unicode.
unicate :: Table -> String
unicate table = boxtop ++ "\n" ++ heading ++ "\n" ++ boxmiddle ++ "\n" ++ body ++ "\n" ++ boxbottom
	where
		((0,0), (rows, columns)) = bounds table
		widths = columnWidths table
		
		boxpart :: String -> String -> String -> String -> String
		boxpart space ljoint midjoint rjoint = 
			ljoint ++ (intercalate midjoint $ map (\x -> repeats space (x+2)) widths) ++ rjoint
		
		boxtop    = boxpart "─" "┌" "┬" "┐"
		boxmiddle = boxpart "─" "├" "┼" "┤"
		boxbottom = boxpart "─" "└" "┴" "┘"
		
		pad :: Integer -> String -> String
		pad n str = str ++ repeats " " (n - genericLength str)
		
		heading, body :: String
		heading            = formatRow 0
		body               = intercalate "\n" $ [ formatRow n | n <- [1..rows]]

		formatRow n        = "│" ++ (intercalate "│" $ zipWith formatCell (rowValues n) widths) ++ "│"
		formatCell v width = " " ++ v ++ repeats " " (width - genericLength v + 1)
		rowValues r        = [ x | x <- map (table!) $ range ((r,0), (r,columns)) ]		
