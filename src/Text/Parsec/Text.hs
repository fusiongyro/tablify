{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- The contents of this module are taken from 
--
--   http://stackoverflow.com/questions/4064532/using-parsec-with-data-text

module Text.Parsec.Text
    ( Parser, GenParser
    ) where

import Text.Parsec.Prim

import qualified Data.Text as T

instance (Monad m) => Stream T.Text m Char where
    uncons = return . T.uncons

type Parser = Parsec T.Text ()
type GenParser t st = Parsec T.Text st
