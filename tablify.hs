-- Generates tables from CSV files.
--
-- I wrote it specifically to make pretty Unicode tables like this:
-- 
-- ┌─────┬─────┬─────┐
-- │ foo │ bar │ baz │
-- ├─────┼─────┼─────┤
-- │ boo │ baa │ bee │
-- │ haa │ hee │ hoo │
-- └─────┴─────┴─────┘
--
-- It always treats the first record as the column headers.

module Main where

-- this declaration lets me forget all about qualifying the UTF8 version of IO
import Prelude hiding (putStr, putStrLn, readFile, getContents) 

import System
import System.IO.UTF8
import System.Console.GetOpt

import Utilities
import Unicode
import HTML
import TBL
import ASCII
import CSV

data Mode = HTML | UTF8 | TBL | ASCII deriving Show
data Options = Options 
	{ optHelp         :: Bool
	, optVersion  :: Bool
	, optMode         :: Mode 
	} deriving Show

defaultOptions = Options
	{ optHelp = False
	, optVersion = False
	, optMode = HTML
	}

usage :: String
usage = "Usage: tablify [OPTION...] file"

options :: [OptDescr (Options -> Options)]
options = 
	[ Option ['v']  ["version"] 
		(NoArg (\o -> o { optVersion = True }))
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
	, Option ['A']  ["ascii"]
		(NoArg (\o -> o { optMode = ASCII }))
		"output ASCII table"
	]

getOptions :: [String] -> IO (Options, [String])
getOptions argv =
	case getOpt Permute options argv of
		(o, n, []) -> return (foldl (flip id) defaultOptions o, n)
		(_, _, errors) -> ioError (userError 
							(concat errors ++ usageInfo usage options))

processOpts :: Table -> Options -> String
processOpts table opts = 
	case optMode opts of
		UTF8  -> unicate table
		HTML  -> htmlify table	
		TBL   -> tblify  table
		ASCII -> asciify table
		
parseData :: String -> IO Table
parseData dat = case parseCSV dat of 
	Left _ -> ioError $ userError "unable to parse file"
	Right result -> return $ stringsToTable result

processFile :: Options -> String -> IO ()
processFile opts file = do
	fileData <- if file == "-" then getContents	else readFile file
	table <- parseData fileData
	putStrLn $ processOpts table opts

showInfo :: Options -> IO ()
showInfo Options { optVersion = True } = putStrLn "tablify version 0.5"
showInfo Options { optHelp = True }    = putStr $ usageInfo usage options

main = do
	args <- getArgs
	(opts, arguments) <- getOptions args
	if optHelp opts || optVersion opts 
		then showInfo opts
		else mapM_ (processFile opts) arguments