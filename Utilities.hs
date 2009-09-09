module Utilities where
import Data.Array
import Data.List

type Table = Array (Integer, Integer) String

repeats :: [a] -> Integer -> [a]
repeats s c = concat $ genericTake c $ repeat s

blankTable :: Integer -> Integer -> Table
blankTable r c = array ((0,0), (r, c)) 
							[ ((i, j), "") | i <- [0..r], j <- [0..c]]

row :: Table -> Integer -> [String]
row table r = [ x | x <- map (table!) $ range ((r,0), (r,columns)) ]
	where
		((0,0), (_, columns)) = bounds table

	
stringsToTable :: [[String]] -> Table
stringsToTable s = basis // dat
	where
		rowC    = genericLength s - 1
		columnC = genericLength (head s) - 1
		basis   = blankTable rowC columnC
		rows    = [ (i, val) | (i, val) <- zip [0..] s ]
		dat     = [ ((i, j), val) | (i, row) <- rows, (j, val) <- zip [0..] row ]
