import List(deleteBy)
import Maybe(fromMaybe)

-- Składnia abstrakcyjna programów

type Identifier = String
type Contents = Integer
type Boolean = Bool
data AExpr = Const Contents | Ident Identifier |
             AExpr AExpr AOp AExpr
data AOp = Oplus | Ominus | Otimes | Odiv | Omod
data BExpr = BTrue | BFalse | BNot BExpr | BExpr BExpr BOp BExpr |
             RExpr AExpr ROp AExpr
data BOp = BAnd | BOr
data ROp = RLt | RLe | RGt | RGe | REq | RNe
data Command = Skip | Identifier := AExpr | Command :. Command |
               If BExpr Command | IfElse BExpr Command Command |
               While BExpr Command

-- Pamięć

class Memory m where
   view :: m -> Identifier -> Contents
   update :: m -> (Identifier,Contents) -> m

-- Funkcja semantyczna

aVal :: Memory m => AExpr -> m -> Contents
bVal :: Memory m => BExpr -> m -> Boolean
cVal :: Memory m => Command -> m -> m

aVal (Const n) _ = n
aVal (Ident x) m = m `view` x
aVal (AExpr e1 op e2) m = aVal e1 m `op'` aVal e2 m where
   op' = case op of
            Oplus -> (+)
            Ominus -> (-)
            Otimes -> (*)
            Odiv -> div
            Omod -> mod

bVal BTrue _ = True
bVal BFalse _ = False
bVal (BNot b) m = not $ bVal b m
bVal (BExpr b1 op b2) m = bVal b1 m `op'` bVal b2 m where
   op' = case op of
            BAnd -> (&&)
            BOr -> (||)
bVal (RExpr e1 op e2) m = aVal e1 m `op'` aVal e2 m where
   op' = case op of
            RLt -> (<)
            RLe -> (<=)
            RGt -> (>)
            RGe -> (>=)
            REq -> (==)
            RNe -> (/=)

cVal Skip m = m
cVal (x := e) m = m `update` (x, aVal e m)
cVal (c1 :. c2) m = cVal c2 . cVal c1 $ m
cVal (If b c) m
   | bVal b m = cVal c m
   | otherwise = m
cVal (IfElse b c1 c2) m
   | bVal b m = cVal c1 m
   | otherwise = cVal c2 m
cVal (While b c) m
   | bVal b m = cVal (While b c) (cVal c m)
   | otherwise = m

-- Implementacja pamięci w postaci listy asocjacji

newtype AssocListMem = ALM [(Identifier, Contents)] deriving Show

instance Memory AssocListMem where
   view (ALM xs) x = fromMaybe 0 (lookup x xs)
   update (ALM xs) p@(x,_) =
      ALM $ p : deleteBy (\ a b -> fst a == fst b) p xs

-- Interpreter i przykładowy program

interpreter :: Command -> AssocListMem
interpreter = flip cVal $ ALM []

sampleProg :: Command
sampleProg =
   ("x" := Const 5) :.
   ("y" := Const 1) :.
   While (RExpr (Ident"x") RGt (Const 1))
      (("y" := AExpr (Ident"x") Otimes (Ident"y")) :.
       ("x" := AExpr (Ident"x") Ominus (Const 1)))

main :: IO ()
main = print $ interpreter sampleProg
