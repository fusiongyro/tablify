{-# LANGUAGE OverloadedStrings #-}

module HTML (converter) where

import qualified Data.Text as T
import Text.XHtml.Strict hiding (header, body)

import Utilities
import Converter

htmlify :: Table -> String
htmlify tbl = prettyHtml $ table << concatHtml [thead << header, tbody << body]
  where
    header = tr << rowToTr th (map T.unpack (head tbl))
    row r  = tr << rowToTr td (map T.unpack r)
    body   = map row $ tail tbl

    rowToTr cellType = map (cellType <<)

converter :: Converter
converter = Converter "HTML" (T.pack . htmlify) "H" "html"
