module Main where

import qualified Text.ParserCombinators.Parsec.Prim as PPrim
import qualified Text.ParserCombinators.Parsec.Char as PChar
import qualified Text.ParserCombinators.Parsec.Combinator as PComb
import qualified Text.ParserCombinators.Parsec.Error as PErr

import Char

kalkulator :: String -> Either PErr.ParseError Int
kalkulator = PPrim.parse expr0 "" where
   expr0 = do
      n1 <- expr1
      ns <- PPrim.many (do
         op <- PChar.oneOf "+-"
         n <- expr1
         return$ case op of
            '+' -> (+ n)
            '-' -> (subtract n))
      return$ foldl (flip ($)) n1 ns
   expr1 = do
      n1 <- expr2
      ns <- PPrim.many (do
         op <- PChar.oneOf "*/%"
         n <- expr2
         return$ case op of
            '*' -> (* n)
            '/' -> (`div` n)
            '%' -> (`mod` n))
      return$ foldl (flip ($)) n1 ns
   expr2 = number PPrim.<|> (do
      PChar.char '('
      n <- expr0
      PChar.char ')'
      return n)
   number = do
      digits <- PComb.many1 PChar.digit
      return$ foldl (\ n c -> 10 * n + digitToInt c) 0 digits
