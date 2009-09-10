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

import Converter
import Utilities
import Unicode
import HTML
import TBL
import ASCII
import CSV

data Options = Options 
	{ optHelp        :: Bool
	, optVersion     :: Bool
	, optConverter   :: Converter
	}

defaultOptions = Options
	{ optHelp      = False
	, optVersion   = False
	, optConverter = HTML.converter
	}

converters = 
	[ ASCII.converter
	, HTML.converter
	, TBL.converter
	, Unicode.converter]

usage :: String
usage = "Usage: tablify [OPTION...] file"

options :: [OptDescr (Options -> Options)]
options = 
	[ Option ['v']  ["version"] 
		(NoArg (\o -> o { optVersion = True }))
		"show version"
	, Option ['h']  ["help"] 
		(NoArg (\o -> o { optHelp = True }))
		"show this help" ] ++ moreOptions
	where
		moreOptions = map converterToOption converters
		converterToOption c@(Converter name conv short long) = 
			Option short [long] 
				(NoArg (\o -> o {optConverter = c}))
				("output " ++ name ++ " table")

getOptions :: [String] -> IO (Options, [String])
getOptions argv =
	case getOpt Permute options argv of
		(o, n, []) -> return (foldl (flip id) defaultOptions o, n)
		(_, _, errors) -> ioError (userError 
							(concat errors ++ usageInfo usage options))

processOpts :: Table -> Options -> String
processOpts table (Options { optConverter = (Converter { cConvert = f })}) = 
	f table
		
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
showInfo Options { optVersion = True } = putStrLn "tablify version 0.6"
showInfo Options { optHelp = True }    = putStr $ usageInfo usage options

main = do
	args <- getArgs
	(opts, arguments) <- getOptions args
	if optHelp opts || optVersion opts 
		then showInfo opts
		else mapM_ (processFile opts) arguments