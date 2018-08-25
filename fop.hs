-- Składnia abstrakcyjna (I rzędu)

type Variable = String
type FunSym = String
type RelSym = String
data Term =
         Term FunSym [Term] |
         Var Variable
data Formula =
         Atomic RelSym [Term] |
         Not Formula |
         Formula Formula Conn Formula |
         Forall Variable Formula |
         Exists Variable Formula
data Conn = And | Or | Impl | Eq

-- Model

class FiniteModel a where
   universe :: [a]
   funInterp :: FunSym -> [a] -> a
   relInterp :: RelSym -> [a] -> Bool

-- Wartościowanie zmiennych

type Valuation a = Variable -> a

modify :: Valuation a -> Variable -> a -> Valuation a
modify val x a y
   | x==y = a
   | otherwise = val y

val0 _ = undefined

-- Interpretacja w modelu

termVal :: FiniteModel a => Term -> Valuation a -> a
termVal (Var x) val = val x
termVal (Term f ts) val = funInterp f $ map (flip termVal val) ts

model :: FiniteModel a => Valuation a -> Formula -> Bool
val `model` Atomic r ts = relInterp r $ map (flip termVal val) ts
val `model` Not f = not $ val `model` f
val `model` Formula f1 op f2 = (val `model` f1) `iop` (val `model` f2)
   where iop = case op of
            And -> (&&)
            Or -> (||)
            Impl -> (\ x y -> not x || y)
            Eq -> (==)
val `model` Forall x f = all (\ a -> modify val x a `model` f) universe
val `model` Exists x f = any (\ a -> modify val x a `model` f) universe

sampleFormula :: Formula
sampleFormula = Forall "x" $ Exists "y" $
   Atomic "=" [Term "*" [Var "x", Var "y"], Term "1" []]

newtype Z5 = Z5 Int
instance FiniteModel Z5 where
   universe = [ Z5 k | k <- [1..4] ]
   funInterp "*" [Z5 i, Z5 j] = Z5 $ (i*j) `mod` 5
   funInterp "1" [] = Z5 1
   relInterp "=" [Z5 i, Z5 j] = i == j

newtype Z6 = Z6 Int
instance FiniteModel Z6 where
   universe = [ Z6 k | k <- [1..5] ]
   funInterp "*" [Z6 i, Z6 j] = Z6 $ (i*j) `mod` 6
   funInterp "1" [] = Z6 1
   relInterp "=" [Z6 i, Z6 j] = i == j

main :: IO ()
main = do
   print $ (val0 :: Variable -> Z5) `model` sampleFormula
   print $ (val0 :: Variable -> Z6) `model` sampleFormula
