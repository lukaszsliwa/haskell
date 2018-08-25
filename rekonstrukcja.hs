{-
   Najprostsza implementacja algorytmu rekonstrukcji typów
   w rachunku lambda z typami prostymi.

   Typy są reprezentowane w postaci trwałych drzew — proste, ale
   mało efektywne — podstawienie działa w czasie O(n). W praktyce
   używa się reprezentacji działających w czasie O(log n) (trwałe)
   lub O(1) (imperatywne).
-}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, PatternGuards #-}

import qualified Data.Map as Map
import qualified Control.Monad.State as State
import qualified Control.Monad.State.Class as StateCl
import qualified Control.Monad.Trans as MTrans
import qualified Text.ParserCombinators.Parsec.Prim as PPrim
import qualified Text.ParserCombinators.Parsec.Token as PTok
import qualified Text.ParserCombinators.Parsec.Language as PLan
import qualified Text.ParserCombinators.Parsec.Combinator as PComb
import qualified Text.ParserCombinators.Parsec.Error as PErr
import qualified System.Console.Readline as Readline
import Monad (liftM, join, when)
import Char (isSpace)
import Prelude hiding (lookup)

-- Lambda termy i ich typy

data LamTerm var =
        Var var |
        LamTerm var :$ LamTerm var |
        Lam var (LamTerm var)

data TypeTerm tyvar =
        TyVar tyvar |
        TypeTerm tyvar :> TypeTerm tyvar

-- Wypisywanie termów i typów

class PrintVar t where
   printVar :: t -> String -> String

instance PrintVar String where
   printVar = showString

instance PrintVar Integer where
   printVar n = showChar 'x' . shows n

instance PrintVar var => Show (LamTerm var) where
   showsPrec _ (Var v) = printVar v
   showsPrec n (t :$ s) = showParen (n >= 11) $
      showsPrec 1 t . showChar ' ' . showsPrec 11 s
   showsPrec n (Lam x t) = showParen (n > 0) $
      showString "\\ " . printVar x . showString " -> " . shows t

instance PrintVar tyVar => Show (TypeTerm tyVar) where
   showsPrec _ (TyVar v) = printVar v
   showsPrec n (t :> s) = showParen (n > 0) $
      showsPrec 1 t . showString " -> " . showsPrec 0 s

-- Czytanie termów

parseLamTerm :: String -> Either PErr.ParseError (LamTerm String)
parseLamTerm = PPrim.parse (do
                  PTok.whiteSpace lexer
                  t <- expr0
                  PComb.eof
                  return t) "console" where
   langDef = PLan.haskellStyle {
      PLan.reservedOpNames = ["\\", "->"]
   }
   lexer = PTok.makeTokenParser langDef
   expr1 = PComb.many1 (PComb.choice [
      PTok.identifier lexer >>= return . Var,
      PTok.parens lexer expr0 ]) >>= return . foldl1 (:$)
   expr0 = PComb.choice [
      expr1,
      do
         PTok.symbol lexer "\\"
         id <- PTok.identifier lexer
         PTok.symbol lexer "->"
         t <- expr0
         return$ Lam id t ]

-- Podstawienia w typach

instance Monad TypeTerm where
   return = TyVar
   TyVar v >>= f = f v
   (t :> s) >>= f = (t >>= f) :> (s >>= f)

type Subst tyVar = tyVar -> TypeTerm tyVar

subst :: Eq tyVar => tyVar -> TypeTerm tyVar -> Subst tyVar
subst v t u
   | v == u = t
   | otherwise = TyVar u

emptySubst :: Subst tyVar
emptySubst = TyVar

o :: Subst tyVar -> Subst tyVar -> Subst tyVar
subst2 `o` subst1 = \ v -> subst1 v >>= subst2

-- Unifikacja typów

occurs :: Eq tyVar => tyVar -> TypeTerm tyVar -> Bool
v `occurs` (TyVar u) = v == u
v `occurs` (t :> s) = v `occurs` t || v `occurs` s

mgu :: (Eq tyVar, PrintVar tyVar, Monad m)
          => TypeTerm tyVar -> TypeTerm tyVar -> m (Subst tyVar)
mgu (t1 :> t2) (s1 :> s2) = do
   subst1 <- mgu t1 s1
   subst2 <- mgu (t2 >>= subst1) (s2 >>= subst1)
   return$ subst2 `o` subst1
mgu (TyVar v) s
   | TyVar u <- s, v == u = return emptySubst
   | otherwise = if v `occurs` s
      then fail $ showString "cycle: " . printVar v
              . showString " occurs in " $ show s
      else return$ subst v s
mgu t s = mgu s t

-- Stan z „licznikiem”

class HasFreshGen tyVar where
   freshTyVar :: Monad m => State.StateT tyVar m tyVar

instance HasFreshGen Integer where
   freshTyVar = do
      st <- StateCl.get
      StateCl.put (st+1)
      return st

instance HasFreshGen String where
   freshTyVar = do
      st@(c:n) <- StateCl.get
      StateCl.put (if c < 'z'
                      then succ c : n
                      else 'a' : if null n
                                    then "1"
                                    else show (read n + 1))
      return st

-- Rekonstrukcja typów

typeOf :: (Monad m, HasFreshGen tyVar, Ord var, Eq tyVar, PrintVar tyVar)
             => LamTerm var -> tyVar -> m (TypeTerm tyVar)
typeOf t = liftM snd . State.evalStateT (typeIt (Map.empty, t)) where
   typeIt (gamma, Var v) = do
      sigma <- Map.lookup v gamma
      return (emptySubst, sigma)
   typeIt (gamma, t1 :$ t2) = do
      (theta1, sigma1) <- typeIt (gamma, t1)
      (theta2, sigma2) <- typeIt (Map.map (>>= theta1) gamma, t2)
      alpha <- freshTyVar
      theta3 <- mgu (sigma1 >>= theta2) (sigma2 :> TyVar alpha)
      return$ (theta3 `o` theta2 `o` theta1, theta3 alpha)
   typeIt (gamma, Lam v t) = do
      alpha <- freshTyVar
      let gamma' = Map.insert v (TyVar alpha) gamma
      (theta, sigma) <- typeIt (gamma', t)
      return (theta, theta alpha :> sigma)

-- Monada błędów

instance Monad (Either String) where
   return = Right
   fail = Left
   (Left x) >>= _ = Left x
   (Right x) >>= f = f x

-- Program główny

main :: IO ()
main = do
   x <- Readline.readline "typeOf> "
   (case x of
      Just s -> do
         let s' = trim s
         when (s' /= "") (do
            Readline.addHistory s
            case parseLamTerm s' of
               Left err -> print err
               Right t -> case typeOf t "a" of
                  Left err -> putStrLn err
                  Right ty -> do
                     putStr $ show t
                     putStr " :: "
                     putStrLn $ show ty)
         main
      Nothing -> putStrLn "") where
   trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- TWI, 2008-06-02
