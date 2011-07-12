{-# LANGUAGE OverloadedStrings #-}

module LaTeX (converter) where

import Prelude hiding (replicate, (++))

import qualified Data.ByteString.Char8 as B

import Utilities
import Converter

-- Some TeX.
texify :: Table -> ByteString
texify table = B.intercalate "\n" [tblstart, body, tblend]
	where
		columns = length $ head table
		tblstart = "\\begin{tabular}" ++ "{" ++ heading 'l' ++ "}"
		tblend = "\\end{tabular}"
		heading c = B.intersperse ' ' $ B.replicate columns c
		body = B.intercalate "\\\\\n" $ map (B.intercalate " & ") table

converter :: Converter
converter = Converter "LaTeX" texify "L" "latex"
