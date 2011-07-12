{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module CSV where

import Control.Monad
import qualified Data.ByteString.Char8 as B
import Text.Parsec
import Text.Parsec.ByteString

import Utilities

bs :: forall st. String -> GenParser Char st ByteString
bs s = string s >>= return . B.pack

-- these CSV file parsing routines courtesy of Real World Haskell:
-- http://book.realworldhaskell.org/read/using-parsec.html

csvFile :: forall st. GenParser Char st [[ByteString]]
csvFile = endBy line eol

line :: forall st. GenParser Char st [ByteString]
line = sepBy cell (char ',')

cell :: forall st. GenParser Char st ByteString
cell = quotedCell <|> (many (noneOf ",\n\r") >>= return . B.pack)

quotedCell :: forall st. GenParser Char st ByteString
quotedCell = 
    do _ <- char '"'
       content <- many quotedChar
       _ <- char '"' <?> "quote at end of cell"
       return $ B.pack content

quotedChar :: forall st. GenParser Char st Char
quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

eol :: forall st. GenParser Char st ByteString
eol =   try (bs "\n\r")
    <|> try (bs "\r\n")
    <|> bs "\n"
    <|> bs "\r"
    <?> "end of line"

parseCSV :: ByteString -> Either ParseError [[ByteString]]
parseCSV = parse csvFile "(unknown)"
