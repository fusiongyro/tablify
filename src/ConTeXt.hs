{-# LANGUAGE OverloadedStrings #-}

module ConTeXt (converter) where

import Prelude hiding ((++))
import qualified Data.ByteString.Char8 as B

import Utilities
import Converter

-- Some ConTeXt.
texify :: Table -> ByteString
texify table = B.intercalate "\n" [tblstart, hl, body, hl, tblend, hl]
	where
		columns = length $ head table
		tblstart = "\\starttable[|" ++ heading ++ "|]"
		tblend = "\\stoptable"
		heading = B.intersperse '|' $ B.replicate columns 'l'
		hl = "\\HL[3]\n"
		body = B.intercalate "\\\\\n" $ map (B.intercalate " & ") table

converter :: Converter
converter = Converter "ConTeXt" texify "C" "context"
