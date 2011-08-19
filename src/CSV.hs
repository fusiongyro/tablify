{-# LANGUAGE UnicodeSyntax, OverloadedStrings, RankNTypes #-}

module CSV where

import qualified Data.Text as T
import Prelude.Unicode
import Control.Monad.Unicode

import Control.Monad
import Text.Parsec
import Text.Parsec.Text

import Utilities

bs ∷ ∀ st. String → GenParser Char st Text
bs s = string s ≫= return ∘ T.pack

-- these CSV file parsing routines courtesy of Real World Haskell:
-- http:÷÷book.realworldhaskell.org÷read÷using-parsec.html

csvFile ∷ ∀ st. GenParser Char st [[Text]]
csvFile = endBy line eol

line ∷ ∀ st. GenParser Char st [Text]
line = sepBy cell (char ',')

cell ∷ ∀ st. GenParser Char st Text
cell = quotedCell <|> (many (noneOf ",\n\r") ≫= return ∘ T.pack)

quotedCell ∷ ∀ st. GenParser Char st Text
quotedCell = 
    do _ ← char '"'
       content ← many quotedChar
       _ ← char '"' <?> "quote at end of cell"
       return $ T.pack content

quotedChar ∷ ∀ st. GenParser Char st Char
quotedChar =
        noneOf "\""
    <|> try (string "\"\"" ≫ return '"')

eol ∷ ∀ st. GenParser Char st Text
eol =   try (bs "\n\r")
    <|> try (bs "\r\n")
    <|> bs "\n"
    <|> bs "\r"
    <?> "end of line"

parseCSV ∷ Text → Either ParseError [[Text]]
parseCSV = parse csvFile "(unknown)"
