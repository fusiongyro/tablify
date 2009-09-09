-- a simple program to take a file with this syntax:
--
--  foo | bar | baz
--  boo | baa | bee
--  haa | hee | hoo
--
-- and generate a nice-looking Unicode table like this:
-- 
-- ┌─────┬─────┬─────┐
-- │ foo │ bar │ baz │
-- ├─────┼─────┼─────┤
-- │ boo │ baa │ bee │
-- │ haa │ hee │ hoo │
-- └─────┴─────┴─────┘
--
-- our basic agenda here is to convert the input into an array then we're
-- going to process the array to discover the maximum widths for each column.
-- once we have these maximums, we'll begin constructing the output by
-- prefixing and suffixing padded strings in the tabular format.
-- we take the first row to be the header. empty cells are permitted.

module Main where

-- this declaration lets me forget all about qualifying the UTF8 version of IO
import Prelude hiding (putStrLn, readFile, getContents) 

import Data.Array
import Data.Char
import Data.List
import System
import System.IO.UTF8
import System.Console.GetOpt
import Text.ParserCombinators.Parsec

type Table = Array (Integer, Integer) String

columnWidths :: Table -> [Integer]
columnWidths table = elems $ foldr combine initial $ assocs table
	where
		((_,_), (rows, columns)) = bounds table
		initial = array (0,columns) [ (i, 0) | i <- [0..columns] ]
		combine ((_,y), val) tab = if genericLength val > tab!y then tab//[(y, genericLength val)] else tab

repeats :: [a] -> Integer -> [a]
repeats s c = concat $ genericTake c $ repeat s

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

-- TBL... yeah...
tblify :: Table -> String
tblify table = tblstart ++ "\n" ++ header ++ "\n" ++ body ++ "\n" ++ tblend
	where
		((0,0), (rows, columns)) = bounds table
		tblstart = ".TS"
		tblend = ".TE"
		header = intercalate " " (genericReplicate (columns+1) "c") ++ "\n" ++ intercalate " " (genericReplicate (columns+1) "l") ++ "."
		body = intercalate "\n" $ [ formatRow n | n <- [1..rows]]
		formatRow n = intercalate "\t" $ rowValues n
		rowValues r        = [ x | x <- map (table!) $ range ((r,0), (r,columns)) ]		

-- these CSV file parsing routines courtesy of Real World Haskell:
-- http://book.realworldhaskell.org/read/using-parsec.html

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell = 
    do char '"'
       content <- many quotedChar
       char '"' <?> "quote at end of cell"
       return content

quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

stringsToTable :: [[String]] -> Table
stringsToTable s = basis // dat
	where
		rowC    = genericLength s - 1
		columnC = genericLength (head s) - 1
		basis   = array ((0,0), (rowC, columnC)) [ ((i,j), "") | i <- [0..rowC], j <- [0..columnC] ]
		rows    = [ (i, val) | (i, val) <- zip [0..] s ]
		dat     = [ ((i, j), val) | (i, row) <- rows, (j, val) <- zip [0..] row ]

data Mode = HTML | UTF8 | TBL deriving Show
data Options = Options 
	{ optHelp         :: Bool
	, optShowVersion  :: Bool
	, optMode         :: Mode 
	} deriving Show

defaultOptions = Options
	{ optHelp = False
	, optShowVersion = False
	, optMode = UTF8
	}

options :: [OptDescr (Options -> Options)]
options = 
	[ Option ['v']  ["version"] 
		(NoArg (\o -> o { optShowVersion = True }))
		"show version"
	, Option ['h']  ["help"] 
		(NoArg (\o -> o { optHelp = True }))
		"show this help"
	, Option ['H']  ["html"]
		(NoArg (\o -> o { optMode = HTML }))
		"output HTML table"
	, Option ['U']  ["utf8"]
		(NoArg (\o -> o { optMode = UTF8 }))
		"output UTF8 table"
	, Option ['T']  ["tbl"]
		(NoArg (\o -> o { optMode = TBL }))
		"output TBL table"
	]

getOptions :: [String] -> IO (Options, [String])
getOptions argv =
	case getOpt Permute options argv of
		(o, n, []) -> return (foldl (flip id) defaultOptions o, n)
		(_, _, errors) -> ioError (userError (concat errors ++ usageInfo header options))
	where
		header = "Usage: tablify [OPTION...] file"

processOpts :: Table -> Options -> String
processOpts table opts = 
	case optMode opts of
		UTF8 -> unicate table
		HTML -> htmlify table	
		TBL  -> tblify  table	
		
parseData :: String -> IO Table
parseData dat = case parseCSV dat of 
	Left _ -> ioError $ userError "unable to parse file"
	Right result -> return $ stringsToTable result

processFile :: Options -> String -> IO ()
processFile opts file = do
	fileData <- if file == "-" then getContents	else readFile file
	table <- parseData fileData
	putStrLn $ processOpts table opts

main = do
	args <- getArgs
	(opts, arguments) <- getOptions args
	mapM_ (processFile opts) arguments	