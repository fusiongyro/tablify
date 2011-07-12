{-# LANGUAGE OverloadedStrings #-}

module HTML (converter) where

import qualified Data.ByteString.Char8 as B
import Text.XHtml.Strict hiding (header, body)

import Utilities
import Converter

htmlify :: Table -> String
htmlify tbl = prettyHtml $ table << concatHtml [thead << header, tbody << body]
  where
    header = tr << rowToTr th (map B.unpack (head tbl))
    row r  = tr << rowToTr td (map B.unpack r)
    body   = map row $ tail tbl

    rowToTr cellType = map (cellType <<)

converter :: Converter
converter = Converter "HTML" (B.pack . htmlify) "H" "html"
