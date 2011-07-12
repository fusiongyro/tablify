{-# LANGUAGE OverloadedStrings #-}

module TBL (converter) where

import Prelude hiding ((++))

import qualified Data.ByteString.Char8 as B

import Utilities
import Converter

-- TBL... yeah...
tblify :: Table -> ByteString
tblify table = B.intercalate "\n" [tblstart, header, body, tblend]
    where
        columns = length $ head table
        tblstart = ".TS"
        tblend = ".TE"
        header = heading 'c' ++ "\n" ++ heading 'l' ++ "."
        heading c = B.intersperse ' ' $ B.replicate (columns+1) c
        body = B.intercalate "\n" $ map (B.intercalate "\t") table

converter :: Converter
converter = Converter "TBL" tblify "T" "tbl"
