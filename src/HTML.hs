module HTML (converter) where

import Text.XHtml.Strict
import Text.Regex
import Utilities
import Converter

htmlify :: Table -> String
htmlify tbl = prettyHtml $ table << (concatHtml [thead << header, tbody << body])
  where
    header = tr << (rowToTr th $ head tbl)
    row r  = tr << (rowToTr td r)
    body   = map row $ tail tbl

    rowToTr cellType = map (cellType <<)

converter = Converter "HTML" htmlify "H" "html"
