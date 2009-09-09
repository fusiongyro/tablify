module Unicode where
import Data.Array
import Data.List hiding (length)
import Prelude hiding (length)

import Utilities

-- let's simplify the reading of the code
length = genericLength

columnWidths :: Table -> [Integer]
columnWidths table = elems $ foldl' combine initial $ assocs table
	where
		((_,_), (rows, columns)) = bounds table
		initial = array (0, columns) [ (i, 0) | i <- [0..columns] ]
		combine tab ((_,y), val) = 
			if length val > tab!y 
				then tab // [(y, length val)] 
				else tab

-- We used to call this ASCII art, but these days it's Unicode.
unicate :: Table -> String
unicate table = concat $ intersperse "\n" [btop, bhead, bmid, body, bbot]
	where
		-- I'm pattern matching the (0,0) as a way of 
		-- checking that the table is valid
		((0,0), (rows, columns)) = bounds table
		widths = columnWidths table
		
		boxpart :: String -> String -> String -> String -> String
		boxpart sp left mid right = 
			   left 
			++ (intercalate mid $ map (\x -> repeats sp (x+2)) widths) 
			++ right
		
		btop = boxpart "─" "┌" "┬" "┐"
		bmid = boxpart "─" "├" "┼" "┤"
		bbot = boxpart "─" "└" "┴" "┘"
		
		pad :: Integer -> String -> String
		pad n str = str ++ repeats " " (n - length str)
		
		bhead, body :: String
		bhead = formatRow 0
		body  = intercalate "\n" $ 
				  [ formatRow n | n <- [1..rows]]

		formatRow n = "│" ++ (intercalate "│" $ 
						zipWith formatCell (row table n) widths) ++ "│"
		formatCell v width = " " ++ v ++ repeats " " (width - length v + 1)
