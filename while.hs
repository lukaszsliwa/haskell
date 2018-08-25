-- Interpreter języka While (parsec + semantyka denotacyjna)

module Main where

import qualified Text.ParserCombinators.Parsec.Token as PTok
import qualified Text.ParserCombinators.Parsec.Language as PLan
import qualified Text.ParserCombinators.Parsec.Char as PChar
import qualified Text.ParserCombinators.Parsec.Combinator as PComb
import qualified Text.ParserCombinators.Parsec.Expr as PExpr
import qualified Text.ParserCombinators.Parsec.Prim as PPrim
import qualified Text.ParserCombinators.Parsec.Error as PErr
import List(deleteBy)
import Maybe(fromMaybe)

-- Składnia abstrakcyjna programów
type Identifier = String
type Contents = Integer
type Boolean = Bool
data AExpr = Const Contents | Ident Identifier |
             AExpr AExpr AOp AExpr deriving Show
data AOp = Oplus | Ominus | Otimes | Odiv | Omod deriving Show
data BExpr = BTrue | BFalse | BNot BExpr | BExpr BExpr BOp BExpr |
             RExpr AExpr ROp AExpr deriving Show
data BOp = BAnd | BOr deriving Show
data ROp = RLt | RLe | RGt | RGe | REq | RNe deriving Show
data Command = Skip | Abort | Identifier := AExpr | Command :. Command |
               If BExpr Command | IfElse BExpr Command Command |
               While BExpr Command deriving Show

-- Parser (100 wierszy kodu)
parse :: String -> Either PErr.ParseError Command
parse = PPrim.parse (do
            PTok.whiteSpace lexer
            c <- program
            PComb.eof
            return c) "stdin" where
   langDef = PLan.LanguageDef {
      PLan.commentStart = "(*",
      PLan.commentEnd = "*)",
      PLan.commentLine = "#",
      PLan.nestedComments = True,
      PLan.identStart = PChar.letter,
      PLan.identLetter = PChar.alphaNum PPrim.<|> PChar.char '_',
      PLan.opStart = PLan.opLetter langDef,
      PLan.opLetter = PChar.oneOf (concat $ PLan.reservedOpNames langDef),
      PLan.reservedOpNames = ["+", "-", "*", "div", "mod", "=", "<>",
                              "<", "<=", ">", ">=", "and", "or", "not"],
      PLan.reservedNames = ["skip", "abort", "if", "then", "else", "fi",
                            "while", "do", "od"],
      PLan.caseSensitive = False }
   lexer = PTok.makeTokenParser langDef
   program = do
      commands <- PTok.semiSep1 lexer command
      return$ case commands of
         [c] -> c
         cs -> foldr1 (:.) cs
   command = PComb.choice [
      PTok.reserved lexer "skip" >> return Skip,
      PTok.reserved lexer "abort" >> return Abort,
      do
         id <- PTok.identifier lexer
         PTok.symbol lexer ":="
         ae <- aExpr
         return$ id := ae,
      do
         PTok.reserved lexer "if"
         be <- bExpr
         PTok.reserved lexer "then"
         th <- program
         PComb.choice [
            PTok.reserved lexer "fi" >> return (If be th),
            do
               PTok.reserved lexer "else"
               el <- program
               PTok.reserved lexer "fi"
               return$ IfElse be th el],
      do
         PTok.reserved lexer "while"
         be <- bExpr
         PTok.reserved lexer "do"
         bo <- program
         PTok.reserved lexer "od"
         return$ While be bo]
   aExpr = PExpr.buildExpressionParser
      [[op "*" PExpr.AssocLeft, op "div" PExpr.AssocLeft,
      op "mod" PExpr.AssocLeft], [op "+" PExpr.AssocLeft,
      op "-" PExpr.AssocLeft ]] primAExpr where
      op name assoc = PExpr.Infix (do
         PTok.reservedOp lexer name
         return (\ x y -> AExpr x (case name of
            "*" -> Otimes
            "div" -> Odiv
            "mod" -> Omod
            "+" -> Oplus
            "-" -> Ominus) y)) assoc
      primAExpr = PComb.choice [
         PTok.integer lexer >>= return . Const,
         PTok.identifier lexer >>= return . Ident,
         PTok.parens lexer aExpr]
   bExpr = PExpr.buildExpressionParser
      [[prefix "not"], [op "and" PExpr.AssocRight],
         [op "or" PExpr.AssocRight]] primBExpr where
      op name assoc = PExpr.Infix (do
         PTok.reservedOp lexer name
         return (\ x y -> BExpr x (case name of
            "and" -> BAnd
            "or" -> BOr) y)) assoc
      prefix name = PExpr.Prefix $ do
         PTok.reservedOp lexer name
         return BNot
      primBExpr = PComb.choice [
         PTok.reserved lexer "true" >> return BTrue,
         PTok.reserved lexer "false" >> return BFalse,
         do
            a1 <- aExpr
            relop <- PComb.choice [
               PChar.string "=",
               PPrim.try (PChar.string "<>"),
               PPrim.try (PChar.string "<="),
               PChar.string "<",
               PPrim.try (PChar.string ">="),
               PChar.string ">"]
            let op = case relop of
                  "=" -> REq
                  "<>" -> RNe
                  "<=" -> RLe
                  "<" -> RLt
                  ">=" -> RGe
                  ">" -> RGt
            a2 <- aExpr
            return$ RExpr a1 op a2,
         PTok.parens lexer bExpr]

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

-- Interpreter
interprete :: Command -> AssocListMem
interprete = flip cVal $ ALM []

-- Główny program
main :: IO ()
main = do
   prog <- getContents
   case parse prog of
      Left err -> print err
      Right ast -> print $ interprete ast
