module Utilities where
import Data.Array
import Data.List

type Table = Array (Integer, Integer) String

repeats :: [a] -> Integer -> [a]
repeats s c = concat $ genericTake c $ repeat s

stringsToTable :: [[String]] -> Table
stringsToTable s = basis // dat
	where
		rowC    = genericLength s - 1
		columnC = genericLength (head s) - 1
		basis   = array ((0,0), (rowC, columnC)) [ ((i,j), "") | i <- [0..rowC], j <- [0..columnC] ]
		rows    = [ (i, val) | (i, val) <- zip [0..] s ]
		dat     = [ ((i, j), val) | (i, row) <- rows, (j, val) <- zip [0..] row ]
