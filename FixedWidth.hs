module FixedWidth
    (Joints, fixedWidthTable) where

import Prelude hiding (length, replicate)
import Data.List hiding (length, replicate)

import Utilities

pad :: Integer -> String -> String
pad n str = str ++ replicate (n - length str) ' '

columnWidths :: Table -> [Integer]
columnWidths table = map (maximum . map length) $ transpose table

boxRow :: [Integer] -> Char -> String -> String -> String -> String
boxRow widths sp left mid right = 
	   left 
	++ (intercalate mid $ map (\x -> replicate (x+2) sp) widths) 
	++ right

type Joints = (String,String,String)

surround :: Char -> String -> String
surround c s = [c] ++ s ++ [c]

-- We used to call this ASCII art, but these days it's Unicode.
fixedWidthTable :: Joints -> Joints -> Joints -> Char -> Char -> Table -> String
fixedWidthTable (tl,tm,tr) (ml, mm, mr) (bl, bm, br) h v table = 
		concat $ intersperse "\n" [btop, bhead, bmid, body, bbot]
	where
		-- I'm pattern matching the (0,0) as a way of 
		-- checking that the table is valid
		widths = columnWidths table
				
		btop = boxRow widths h tl tm tr
		bmid = boxRow widths h ml mm mr
		bbot = boxRow widths h bl bm br
		
		bhead, body :: String
		bhead = formatRow (head table)
		body  = intercalate "\n" $ map formatRow (tail table)

		formatRow row = surround v $ intercalate [v] $ 
						zipWith formatCell row widths
		formatCell :: String -> Integer -> String
		formatCell val width = surround ' ' $ pad width val
