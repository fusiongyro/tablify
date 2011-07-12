{-# LANGUAGE OverloadedStrings #-}

module LaTeX (converter) where

import Prelude hiding (replicate, (++))

import Utilities
import Converter

-- Some TeX.
texify :: Table -> Text
texify table = intercalate "\n" [tblstart, body, tblend]
	where
		columns = length $ head table
		tblstart = "\\begin{tabular}" ++ "{" ++ heading "l" ++ "}"
		tblend = "\\end{tabular}"
		heading c = intersperse ' ' $ replicate columns c
		body = intercalate "\\\\\n" $ map (intercalate " & ") table

converter :: Converter
converter = Converter "LaTeX" texify "L" "latex"
