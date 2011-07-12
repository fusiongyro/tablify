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

import Prelude
import qualified Data.ByteString as B
import System.Environment
import System.IO
import System.Console.GetOpt

import Converter
import Utilities hiding ((++))
import Unicode
import HTML
import TBL
import ASCII
import CSV
import LaTeX
import ConTeXt

version :: Double
version = 0.8

data Options = Options 
    { optHelp        :: Bool
    , optVersion     :: Bool
    , optConverter   :: Converter
    }

defaultOptions :: Options
defaultOptions = Options
    { optHelp      = False
    , optVersion   = False
    , optConverter = HTML.converter
    }

converters :: [Converter]
converters = 
    [ ASCII.converter
    , HTML.converter
    , TBL.converter
    , Unicode.converter
    , LaTeX.converter
    , ConTeXt.converter]

usage :: String
usage = "Usage: tablify [OPTION...] file"

options :: [OptDescr (Options -> Options)]
options = 
    [ Option "v"  ["version"] 
        (NoArg (\o -> o { optVersion = True }))
        "show version"
    , Option "h"  ["help"] 
        (NoArg (\o -> o { optHelp = True }))
        "show this help" ] ++ moreOptions
    where
        moreOptions = map converterToOption converters
        converterToOption c@(Converter name _ short long) = 
            Option short [long] 
                (NoArg (\o -> o {optConverter = c}))
                ("output " ++ name ++ " table")

getOptions :: [String] -> IO (Options, [String])
getOptions argv =
    case getOpt Permute options argv of
        (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
        (_, _, errors) -> ioError (userError 
                            (concat errors ++ usageInfo usage options))

processOpts :: Table -> Options -> ByteString
processOpts table (Options _ _ (Converter { cConvert = f })) = f table
        
parseData :: ByteString -> IO Table
parseData dat = case parseCSV dat of 
    Left _ -> ioError $ userError "unable to parse file"
    Right result -> return result

processFile :: Options -> String -> IO ()
processFile opts file = do
    fileData <- if file == "-" then B.getContents else B.readFile file
    table <- parseData fileData
    B.putStrLn $ processOpts table opts

showInfo :: Options -> IO ()
showInfo Options { optVersion = True } = putStrLn $ "tablify version " ++ show version
showInfo Options { optHelp = True }    = putStr $ usageInfo usage options
showInfo _                             = fail "unable to show info for weird options"

main :: IO ()
main = do
    args <- getArgs
    (opts, arguments) <- getOptions args
    hSetEncoding stdout utf8
    if optHelp opts || optVersion opts 
        then showInfo opts
        else mapM_ (processFile opts) arguments
