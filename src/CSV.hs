{-# LANGUAGE RankNTypes #-}

module CSV where
import Text.ParserCombinators.Parsec

-- these CSV file parsing routines courtesy of Real World Haskell:
-- http://book.realworldhaskell.org/read/using-parsec.html

csvFile :: forall st. GenParser Char st [[String]]
csvFile = endBy line eol

line :: forall st. GenParser Char st [String]
line = sepBy cell (char ',')

cell :: forall st. GenParser Char st String
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell :: forall st. GenParser Char st String
quotedCell = 
    do _ <- char '"'
       content <- many quotedChar
       _ <- char '"' <?> "quote at end of cell"
       return content

quotedChar :: forall st. GenParser Char st Char
quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

eol :: forall st. GenParser Char st String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvFile "(unknown)"
