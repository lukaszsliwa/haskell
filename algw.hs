-- ALGEBRY WIELOGATUNKOWE

{-# OPTIONS_GHC -fglasgow-exts #-}

import qualified Data.Map as Map
import Maybe (fromMaybe)

-- Listy polimorficzne
-- Lista typu List ty (s1 :. s2 :. ... sn :. Nil)
-- zawiera elementy typów ty s1, ty s2, ... ty sn

class PList (plist :: *)
data Nil
data PList tl => hd :. tl
instance PList Nil
instance PList tl => PList (hd :. tl)
data List ty xs where
   Nil :: List ty Nil
   (:.) :: (Sort hd, PList tl)
              => ty hd -> List ty tl -> List ty (hd :. tl)
infixr 5 :.

instance Show (List ty Nil) where
   show Nil = ""
instance Show (ty hd) => Show (List ty (hd :. Nil)) where
   show (hd :. Nil) = "(" ++ show hd ++ ")"
instance (Show (ty hd), Show (List ty tl))
            => Show (List ty (hd :. tl)) where
   show (hd :. tl) = "(" ++ show hd ++ "," ++ tail (show tl)

pMap :: (forall s. Sort s => a s -> b s) -> List a xs -> List b xs
pMap _ Nil = Nil
pMap f (x :. xs) = f x :. pMap f xs

-- Termy

class Sort (sort :: *) -- fantomowy parametr termu

data Term sig var sort where
   Term :: Sort sort => sig args sort -> List (Term sig var) args
              -> Term sig var sort
   Var :: Sort sort => var sort -> Term sig var sort

type VarVal var universe =
   forall sort. Sort sort => var sort -> universe sort

class Algebra universe sig where
   interp :: Sort sort =>
      sig args sort -> List universe args -> universe sort

termVal :: (Algebra univ sig, Sort sort)
              => Term sig var sort -> VarVal var univ -> univ sort
termVal (Var var) varVal = varVal var
termVal (Term f xs) varVal = interp f $ pMap (\ t -> termVal t varVal) xs

instance Algebra (Term sig var) sig where
   interp = Term 

-- Przykładowa sygnatura -- język While

data ASort
data BSort
data CSort
data ISort
instance Sort ASort
instance Sort BSort
instance Sort CSort
instance Sort ISort

infixr 4 :->

data (:->) :: * -> * -> * where
   Ident :: String -> (Nil :-> ISort)
   Num :: Integer -> (Nil :-> ASort)
   DeRef :: ISort :. Nil :-> ASort
   Oplus :: ASort :. ASort :. Nil :-> ASort
   Ominus :: ASort :. ASort :. Nil :-> ASort
   Otimes :: ASort :. ASort :. Nil :-> ASort
   Odiv :: ASort :. ASort :. Nil :-> ASort
   Omod :: ASort :. ASort :. Nil :-> ASort
   BTrue :: Nil :-> BSort
   BFalse :: Nil :-> BSort
   Lt :: ASort :. ASort :. Nil :-> BSort
   Leq :: ASort :. ASort :. Nil :-> BSort
   Gt :: ASort :. ASort :. Nil :-> BSort
   Geq :: ASort :. ASort :. Nil :-> BSort
   Neq :: ASort :. ASort :. Nil :-> BSort
   Eq :: ASort :. ASort :. Nil :-> BSort
   BNeg :: BSort :. Nil :-> BSort
   BOr :: BSort :. BSort :. Nil :-> BSort
   BAnd :: BSort :. BSort :. Nil :-> BSort
   Skip :: Nil :-> CSort
   Abort :: Nil :-> CSort
   Assgn :: ISort :. ASort :. Nil :-> CSort
   Comp :: CSort :. CSort :. Nil :-> CSort
   If :: BSort :. CSort :. Nil :-> CSort
   IfElse :: BSort :. CSort :. CSort :. Nil :-> CSort
   While :: BSort :. CSort :. Nil :-> CSort

type Mem = Map.Map String Integer

data Universe sort where
   AUnv :: (Mem -> Integer) -> Universe ASort
   BUnv :: (Mem -> Bool) -> Universe BSort
   CUnv :: (Mem -> Mem) -> Universe CSort
   IUnv :: String -> Universe ISort

instance Algebra Universe (:->) where
   interp (Ident s) Nil = IUnv s
   interp (Num n) Nil = AUnv (const n)
   interp DeRef (IUnv s :. Nil) =
      AUnv (\m -> fromMaybe 0 $ Map.lookup s m)
   interp Oplus (AUnv f :. AUnv g :. Nil) = AUnv (\m -> f m + g m)
   interp Ominus (AUnv f :. AUnv g :. Nil) = AUnv (\m -> f m - g m)
   interp Otimes (AUnv f :. AUnv g :. Nil) = AUnv (\m -> f m * g m)
   interp Odiv (AUnv f :. AUnv g :. Nil) = AUnv (\m -> f m `div` g m)
   interp Omod (AUnv f :. AUnv g :. Nil) = AUnv (\m -> f m `mod` g m)
   interp BTrue Nil = BUnv (const True)
   interp BFalse Nil = BUnv (const False)
   interp Lt (AUnv f :. AUnv g :. Nil) = BUnv (\m -> f m < g m)
   interp Gt (AUnv f :. AUnv g :. Nil) = BUnv (\m -> f m > g m)
   interp Leq (AUnv f :. AUnv g :. Nil) = BUnv (\m -> f m <= g m)
   interp Geq (AUnv f :. AUnv g :. Nil) = BUnv (\m -> f m >= g m)
   interp Neq (AUnv f :. AUnv g :. Nil) = BUnv (\m -> f m /= g m)
   interp Eq (AUnv f :. AUnv g :. Nil) = BUnv (\m -> f m == g m)
   interp BNeg (BUnv f :. Nil) = BUnv (not . f)
   interp BOr (BUnv f :. BUnv g :. Nil) = BUnv (\m -> f m || g m)
   interp BAnd (BUnv f :. BUnv g :. Nil) = BUnv (\m -> f m && g m)
   interp Skip Nil = CUnv id
   interp Abort Nil = CUnv undefined
   interp Assgn (IUnv x :. AUnv f :. Nil) =
      CUnv (\m -> Map.insert x (f m) m)
   interp Comp (CUnv f :. CUnv g :. Nil) = CUnv $ g . f
   interp If (BUnv f :. CUnv g :. Nil) =
      CUnv (\m -> if f m then g m else m)
   interp IfElse (BUnv f :. CUnv g :. CUnv h :. Nil) =
      CUnv (\m -> if f m then g m else h m)
   interp While (BUnv f :. CUnv g :. Nil) =
      CUnv (\m -> if f m
         then case interp While (BUnv f :. CUnv g :. Nil) of
                 CUnv h -> h (g m)
         else m)

-- Zbiór pusty (zmiennych gatunku sort)

data EmptySet sort
emptyVarVal :: VarVal EmptySet universe
emptyVarVal = undefined

-- Przykładowy program

type WhileProg = Term (:->) EmptySet CSort

sampleProg :: WhileProg
sampleProg = Term Comp (Term Assgn (Term (Ident "x") Nil
    :. Term (Num 5) Nil :. Nil) :.
    Term Comp (Term Assgn (Term (Ident "y") Nil :.
    Term (Num 1) Nil :. Nil) :.
    Term While (Term Gt (Term DeRef (Term (Ident "x") Nil :. Nil) :.
    Term (Num 1) Nil :. Nil) :.
    Term Comp (Term Assgn (Term (Ident "y") Nil :. Term Otimes
    (Term DeRef (Term (Ident "x") Nil :. Nil) :.
    Term DeRef (Term (Ident "y") Nil :. Nil) :. Nil) :. Nil) :.
    Term Assgn (Term (Ident "x") Nil :. Term Ominus
    (Term DeRef(Term (Ident "x") Nil :. Nil) :.
    Term (Num 1) Nil :. Nil) :. Nil) :.  Nil) :. Nil) :. Nil) :. Nil)

sampleResult :: Mem
sampleResult =
   case termVal sampleProg emptyVarVal of
      CUnv f -> f Map.empty

main :: IO ()
main = print sampleResult
