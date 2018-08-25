{-#
   OPTIONS_GHC
      -fglasgow-exts
      -fallow-undecidable-instances
      -fallow-overlapping-instances
#-}

import Text.Show.Functions

-- Polymorphic lists

class PList (plist :: *)
data Nil
data PList tail => head :. tail
instance PList Nil
instance PList tail => PList (head :. tail)
data List :: * -> * where
   Nil :: List Nil
   (:.) :: PList tail => head -> List tail -> List (head :. tail)
infixr 5 :.

-- Show

instance Show (List Nil) where
   show Nil = "[]"
instance Show elem => Show (List (elem :. Nil)) where
   show (elem :. Nil) = "[" ++ show elem ++ "]"
instance (Show hd, Show (List tl)) => Show (List (hd :. tl)) where
   show (hd :. tl) = "[" ++ show hd ++ "," ++ tail (show tl)

-- Length

pLength :: List a -> Int
pLength Nil = 0
pLength (_ :. xs) = 1 + pLength xs

-- Concatenation

class (PList xs, PList ys, PList zs)
         => Concat xs ys zs | xs ys -> zs where
   (+.+) :: List xs -> List ys -> List zs
instance PList ys => Concat Nil ys ys where
   Nil +.+ ys = ys
instance Concat xs ys zs => Concat (x :. xs) ys (x :. zs) where
   (x :. xs) +.+ ys = x :. (xs +.+ ys)
infixr 5 +.+

-- Reversing

class (PList xs, PList ys) => Reverse xs ys | xs -> ys where
   pReverse :: List xs -> List ys
-- instance Reverse ...   --- left as a homework

-- Test

main :: IO ()
main = do
   prnt "xs" xs
   prnt "ys" ys
   prnt "xs +.+ ys" zs where
      xs = 'a' :. True :. 1 :. (5 :. 6 :. Nil) :. Nil
      ys = id :. ('b',False) :. Nil
      zs = xs +.+ ys
      prnt ss us = putStrLn $ ss ++ " = " ++ show us
                                 ++ " has length " ++ show (pLength us)

-- TWI, Apr 12, 2008
