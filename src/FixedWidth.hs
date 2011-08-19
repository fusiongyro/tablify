{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}

module FixedWidth
    (Joints, fixedWidthTable) where

import Prelude hiding (length, replicate, (++))
import Data.List hiding (length, replicate, intercalate, (++))

import qualified Data.Text as T

import Utilities

pad :: Int -> Text -> Text
pad n str = str ++ replicate (n - T.length str) " "

-- given a table, return a list of the maximum widths of each column
columnWidths :: Table -> [Int]
columnWidths table = map (maximum . map T.length) $ transpose table

boxRow :: [Int] -> Char -> Text -> Text -> Text -> Text
boxRow widths sp left mid right = 
       left 
    ++ intercalate mid (map (\x -> replicate (x+2) (T.singleton sp)) widths)
    ++ right

type Joints = (Text,Text,Text)

surround :: Char -> Text -> Text
surround c s = (c `T.cons` s) `T.snoc` c

-- We used to call this ASCII art, but these days it's Unicode.
fixedWidthTable :: Joints -> Joints -> Joints -> Char -> Char -> Table -> Text
fixedWidthTable (tl,tm,tr) (ml, mm, mr) (bl, bm, br) h v table = 
        intercalate "\n" [btop, bhead, bmid, body, bbot]
    where
        -- I'm pattern matching the (0,0) as a way of 
        -- checking that the table is valid
        widths = columnWidths table
                
        btop = boxRow widths h tl tm tr
        bmid = boxRow widths h ml mm mr
        bbot = boxRow widths h bl bm br
        
        bhead, body :: Text
        bhead = formatRow (head table)
        body  = intercalate "\n" $ map formatRow (tail table)

        formatRow row = surround v $ intercalate (T.singleton v) $ 
                        zipWith formatCell row widths
        formatCell :: Text -> Int -> Text
        formatCell val width = surround ' ' $ pad width val
