module FixedWidth where
import Data.Array
import Data.List hiding (length)
import Prelude hiding (length)

import Utilities

-- let's simplify the reading of the code
length = genericLength

pad :: Integer -> String -> String
pad n str = str ++ repeats " " (n - length str)

columnWidths :: Table -> [Integer]
columnWidths table = elems $ foldl' combine initial $ assocs table
	where
		((_,_), (rows, columns)) = bounds table
		initial = array (0, columns) [ (i, 0) | i <- [0..columns] ]
		combine tab ((_,y), val) = 
			if length val > tab!y 
				then tab // [(y, length val)] 
				else tab

--
boxpart :: [Integer] -> String -> String -> String -> String -> String
boxpart widths sp left mid right = 
	   left 
	++ (intercalate mid $ map (\x -> repeats sp (x+2)) widths) 
	++ right

--

type Joints = (String,String,String)

-- We used to call this ASCII art, but these days it's Unicode.
fixedWidthTable :: Joints -> Joints -> Joints -> String -> String -> Table -> String
fixedWidthTable (tl,tm,tr) (ml, mm, mr) (bl, bm, br) h v table = 
		concat $ intersperse "\n" [btop, bhead, bmid, body, bbot]
	where
		-- I'm pattern matching the (0,0) as a way of 
		-- checking that the table is valid
		((0,0), (rows, columns)) = bounds table
		widths = columnWidths table
				
		btop = boxpart widths h tl tm tr
		bmid = boxpart widths h ml mm mr
		bbot = boxpart widths h bl bm br
		
		bhead, body :: String
		bhead = formatRow 0
		body  = intercalate "\n" $ 
				  [ formatRow n | n <- [1..rows]]

		formatRow n = v ++ (intercalate v $ 
						zipWith formatCell (row table n) widths) ++ v
		formatCell v width = " " ++ v ++ repeats " " (width - length v + 1)
