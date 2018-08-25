{-#
   OPTIONS_GHC
      -fglasgow-exts
      -fallow-undecidable-instances
      -fallow-overlapping-instances
#-}

import Monad (foldM, mplus, guard, MonadPlus(mzero,mplus))
import List (intersperse)
import Maybe (fromMaybe)
import qualified Data.Map as Map
import Char (isAlphaNum, isLower, isUpper, isSpace)
import IO (isEOF)

-- OBLICZENIA Z MOŻLIWYM NIEPOWODZENIEM

data Failable a = Failure String | Success a
instance Monad Failable where
   return = Success
   fail = Failure
   Failure l >>= _ = Failure l
   (Success a) >>= f = f a
instance MonadPlus Failable where
   mzero = Failure ""
   (Failure _) `mplus` r = r
   l@(Success _) `mplus` _ = l
instance Show a => Show (Failable a) where
   show (Failure s) = "Error: " ++ s
   show (Success a) = "Success: " ++ show a

-- TERMY
-- Składnia abstrakcyjna termów:

class Sig sig where
   arity :: sig -> Int
   chkArity :: MonadPlus m => sig -> Int -> m sig
   chkArity f n = do
      guard$ arity f == n
      return f

data Sig sig => Term sig var = Var var | FunSym sig [Term sig var]

-- Składnia konkretna termów:

instance (Sig sig, Show var, Show sig) => Show (Term sig var) where
   show (Var var) = show var
   show (FunSym f ts)
      | null ts = show f
      | otherwise = show f ++ "(" ++
           (concat . intersperse "," $ map show ts) ++ ")"

instance (Sig sig, Read var, Read sig) => Read (Term sig var) where
   readsPrec p str =
      do
         (v, rest) <- readsPrec p str
         return (Var v, rest)
      `mplus` do
      (f, rest) <- readsPrec p str
      do
            ("(", args) <- lex rest
            (ts, rest') <- getArgs ("," ++ args)
            r <- funSym f ts
            return (r, rest')
         `mplus` do
            r <- funSym f []
            return (r, rest) where
      getArgs str =
         do
            (",", str') <- lex str
            (t, rest) <- readsPrec p str'
            (ts, rest') <- getArgs rest
            return (t:ts, rest')
         `mplus` do
            (")", rest) <- lex str
            return ([], rest)
      funSym f ts = do
         f' <- chkArity f (length ts)
         return$ FunSym f' ts

-- ALGEBRY

class Sig sig => Algebra universe sig where
   interp :: sig -> [universe] -> universe

type Valuation var universe = var -> universe

termVal :: (Sig sig, Algebra universe sig)
              => Valuation var universe -> Term sig var -> universe
termVal val (Var var) = val var
termVal val (FunSym f ts) = interp f $ map (termVal val) ts

-- Algebra termów

instance Sig sig => Algebra (Term sig var) sig where
   interp = FunSym

-- Algebra termów stałych

data Empty
type GroundTerm sig = Term sig Empty

-- PODSTAWIENIA (odwzorowania ze zmiennych w termy)

class VarMap m sig var where
   emptyVarMap :: m sig var
   assign :: var -> Term sig var -> m sig var -> m sig var
   lookUp :: m sig var -> var -> Term sig var
   fromAssocList :: [(var, Term sig var)] -> m sig var
   fromAssocList = foldr (uncurry assign) emptyVarMap
   toAssocList :: m sig var -> [(var, Term sig var)]

instance (Sig sig, Show sig, Show var, VarMap m sig var)
            => Show (m sig var) where
   show = ("[" ++) . (++ "]") . concat . intersperse ","
          . map (\(x,t) -> show x ++ "/" ++ show t) . toAssocList

-- Rozszerzenie podstawień

class Subst t sig var where
   subst :: VarMap m sig var => m sig var -> t -> t

-- na termy

instance Sig sig => Subst (Term sig var) sig var where
   subst s (Var v) = lookUp s v
   subst s (FunSym f ts) = FunSym f $ map (subst s) ts

-- na pary termów

instance Subst t sig var => Subst (t,t) sig var where
   subst s (t1,t2) = (subst s t1, subst s t2)

-- UNIFIKACJA
-- Wystąpienia zmiennych w termach

occursIn :: (Eq var, Sig sig) => var -> Term sig var -> Bool
occursIn v (Var v') = v == v'
occursIn v (FunSym _ ts) = any (occursIn v) ts

-- Zadania unifikacji

newtype Unif sig var = Unif { unUnif :: (Term sig var, Term sig var) }
instance (Sig sig, Show sig, Show var) => Show (Unif sig var) where
   show (Unif (t1, t2)) = show t1 ++ " = " ++ show t2
instance (Sig sig, Read sig, Read var) => Read (Unif sig var) where
   readsPrec p str = do
      (t1, rest) <- readsPrec p str
      ("=", rest') <- lex rest
      (t2, rest'') <- readsPrec p rest'
      return (Unif (t1, t2), rest'')

-- Algorytm unifikacji

mgu :: (Monad m, Sig sig, VarMap s sig var, Eq sig, Eq var)
          => Unif sig var -> m (s sig var)
mgu = unify emptyVarMap . unUnif where
   unify sub tt =
      case subst sub tt of
         (FunSym f ts, FunSym f' ts') ->
            if f /= f'
               then fail "Symbol clash"
               else foldM unify sub (zip ts ts')
         (Var v, t@(Var v')) ->
            if v == v'
               then return sub
               else return$ assign v t sub
         (Var v, t) ->
            if v `occursIn` t
               then fail "Cycle"
               else return$ assign v t sub
         (t1, t2) -> unify sub (t2, t1)

-- IMPLEMENTACJE
-- Implementacja podstawień

newtype MapVar sig var =
   MapVar {unMapVar :: Map.Map var (Term sig var)}
instance (Ord var, Sig sig) => VarMap MapVar sig var where
   emptyVarMap = MapVar Map.empty
   assign v t = MapVar
                . Map.insert v t
                . Map.map (subst . MapVar $ Map.singleton v t)
                . unMapVar
   lookUp (MapVar m) v = fromMaybe (Var v) (Map.lookup v m)
   fromAssocList = MapVar . Map.fromList
   toAssocList = Map.toList . unMapVar

-- Implementacja sygnatur i zmiennych

readIdent :: (Char -> Bool) -> (String -> a) -> ReadS a
readIdent fstChar wrap str = do
   (c:_) <- return ident
   guard$ fstChar c
   return (wrap ident, dropWhile isAlphaNum str') where
      str' = dropWhile isSpace str
      ident = takeWhile isAlphaNum str'

newtype IVar = IVar { unIVar :: String } deriving (Eq, Ord)
instance Show IVar where
   show = unIVar
instance Read IVar where
   readsPrec _ = readIdent isUpper IVar

data IFun = IFun { name :: String, ar :: Int } deriving (Eq, Ord)
instance Sig IFun where
   arity = ar
   chkArity f n
      | arity f < 0 || arity f == n = return$ f { ar = n }
      | otherwise = mzero
instance Show IFun where
   show = name
instance Read IFun where
   readsPrec _ = readIdent isLower (flip IFun (-1))

-- PROGRAM GŁÓWNY --- ROZWIĄZYWACZ ZADAŃ UNIFIKACJI

solve :: String -> String
solve line = show $ ((mgu $ read line) :: Failable (MapVar IFun IVar))

main :: IO ()
main = do
   finish <- isEOF
   if finish
      then return ()
      else do
         line <- getLine
         putStrLn $ solve line
         main
