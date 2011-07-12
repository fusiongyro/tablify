{-# LANGUAGE OverloadedStrings #-}

module TBL (converter) where

import Prelude hiding ((++), replicate)

import qualified Data.ByteString.Char8 as B

import Utilities
import Converter

-- TBL... yeah...
tblify :: Table -> ByteString
tblify table = intercalate "\n" [tblstart, header, body, tblend]
    where
        columns = length $ head table
        tblstart = ".TS"
        tblend = ".TE"
        header = heading 'c' ++ "\n" ++ heading 'l' ++ "."
        heading c = intersperse ' ' $ replicate (columns+1) c
        body = intercalate "\n" $ map (intercalate "\t") table

converter :: Converter
converter = Converter "TBL" tblify "T" "tbl"
