import Monad (MonadPlus, mzero, mplus)
import Char (digitToInt)

import qualified Control.Monad.State as State
import qualified Control.Monad.State.Class as StateCl
import qualified Control.Monad.Trans as MTrans

type Parser token m value = State.StateT [token] m value

parse :: Monad m => Parser token m value -> [token] -> m value
parse = State.evalStateT 

isElem :: (Eq token, MonadPlus m) => [token] -> Parser token m token
isElem tokens = do
   ts <- StateCl.get
   case ts of
      (t : ts') | t `elem` tokens -> do
         StateCl.put ts'
         return t
      _ -> mzero

isEmpty :: MonadPlus m => Parser token m ()
isEmpty = do
   ts <- StateCl.get
   if null ts
      then return ()
      else mzero

many :: MonadPlus m => Parser token m value -> Parser token m [value]
many parse =
   do
      v <- parse
      vs <- many parse
      return$ v:vs
   `mplus` return []

many1 :: MonadPlus m => Parser token m value -> Parser token m [value]
many1 parse = do
   v <- parse
   vs <- many parse
   return$ v:vs

option :: MonadPlus m
   => Parser token m value -> Parser token m (Maybe value)
option parser = (parser >>= return . Just) `mplus` return Nothing

number :: MonadPlus m => Parser Char m Integer
number = many1 (isElem "0123456789") >>=
   return . foldl (\ n x -> 10 * n + fromIntegral (digitToInt x)) 0

kalkulator :: MonadPlus m => String -> m Integer
kalkulator = parse (do
   n <- expr0
   isEmpty
   return n) where
   expr0 = do
      e1 <- expr1
      many (do
         op <- isElem "+-"
         e2 <- expr1
         return$ case op of
            '+' -> (+ e2)
            '-' -> subtract e2) >>= return . foldr ($) e1
   expr1 = do
      e1 <- expr2
      many (do
         op <- isElem "*/%"
         e2 <- expr2
         return$ case op of
            '*' -> (* e2)
            '/' -> (`div` e2)
            '%' -> (`mod` e2)) >>= return . foldr ($) e1
   expr2 =
      do
         isElem "("
         e <- expr0
         isElem ")"
         return e
      `mplus` number

rozbierz :: MonadPlus m => String -> m String
rozbierz = parse (do
   z <- zdanie
   isEmpty
   return z) . words where
   zdanie = do
      r <- rzeczownik
      c <- czasownik
      d <- option dopelnienie
      return$ r ++ " " ++ c ++ case d of
         Nothing -> ""
         Just d -> " (" ++ d ++ ")"
   rzeczownik = do
      a <- option (isElem ["a"])
      r <- isElem ["John", "man", "stick"]
      return$ case a of
         Nothing -> r
         Just a -> a ++ " " ++ r
   czasownik = do
      c <- isElem ["hit"]
      p <- przech
      return$ c ++ " " ++ p
   przech = do
      r <- rzeczownik
      d <- option dopelnienie
      return$ "(" ++ r ++ (case d of
         Nothing -> ""
         Just d -> " " ++ d) ++ ")"
   dopelnienie = do
      w <- isElem ["with"]
      r <- rzeczownik
      return$ w ++ " " ++ r

wynik :: [String]
wynik = rozbierz "John hit a man with a stick"
