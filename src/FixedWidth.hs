{-# LANGUAGE OverloadedStrings #-}

module FixedWidth
    (Joints, fixedWidthTable) where

import Prelude hiding (length, replicate, (++))
import Data.List hiding (length, replicate, (++))
import qualified Data.ByteString.Char8 as B

import Utilities

pad :: Int -> ByteString -> ByteString
pad n str = str ++ B.replicate (n - B.length str) ' '

-- given a table, return a list of the maximum widths of each column
columnWidths :: Table -> [Int]
columnWidths table = map (maximum . map B.length) $ transpose table

boxRow :: [Int] -> Char -> ByteString -> ByteString -> ByteString -> ByteString
boxRow widths sp left mid right = 
       left 
    ++ B.intercalate mid (map (\x -> B.replicate (x+2) sp) widths)
    ++ right

type Joints = (ByteString,ByteString,ByteString)

surround :: Char -> ByteString -> ByteString
surround c s = (c `B.cons` s) `B.snoc` c

-- We used to call this ASCII art, but these days it's Unicode.
fixedWidthTable :: Joints -> Joints -> Joints -> Char -> Char -> Table -> ByteString
fixedWidthTable (tl,tm,tr) (ml, mm, mr) (bl, bm, br) h v table = 
        B.intercalate "\n" [btop, bhead, bmid, body, bbot]
    where
        -- I'm pattern matching the (0,0) as a way of 
        -- checking that the table is valid
        widths = columnWidths table
                
        btop = boxRow widths h tl tm tr
        bmid = boxRow widths h ml mm mr
        bbot = boxRow widths h bl bm br
        
        bhead, body :: ByteString
        bhead = formatRow (head table)
        body  = B.intercalate "\n" $ map formatRow (tail table)

        formatRow row = surround v $ B.intercalate (B.singleton v) $ 
                        zipWith formatCell row widths
        formatCell :: ByteString -> Int -> ByteString
        formatCell val width = surround ' ' $ pad width val
