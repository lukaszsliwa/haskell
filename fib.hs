{-#
    OPTIONS_GHC
       -fglasgow-exts
       -fallow-undecidable-instances
       -fallow-incoherent-instances
#-}

fib1 :: (Num a, Num n) => n -> a
fib1 0 = 1
fib1 1 = 1
fib1 n = fib1 (n-1) + fib1 (n-2)

class Monoid a where
   plus :: a -> a -> a
   zero :: a
    --    Aksjomaty:
    --    (x `plus` y) `plus` z = x `plus` (y `plus` z)
    --    zero `plus` x = x
    --    x `plus` zero = z

instance Num a => Monoid a where
   plus = (+)
   zero = 0

instance Monoid [a] where
   plus = (++)
   zero = []

fib2 :: (Monoid a, Num n) => a -> a -> n -> a
fib2 f0 f1 0 = f0
fib2 f0 f1 1 = f1
fib2 f0 f1 n = fib2 f1 (f0 `plus` f1) (n-1)

fib2i :: (Num a, Num b) => a -> b
fib2i = fib2 1 1

fib2s :: Num a => a -> String
fib2s = fib2 "a" "b"

power :: (Monoid a, Integral t) => a -> t -> a
power x 0 = zero
power x n
   | n `mod` 2 == 0 = y `plus` y
   | otherwise      = y `plus` y `plus` x where
        y = power x (n `div` 2)

type Matrix2x2 a = ((a,a),(a,a))

instance Num a => Monoid (Matrix2x2 a) where
   zero = ((1,0), (0,1))
   ((a11,a12),(a21,a22)) `plus` ((b11,b12),(b21,b22)) =
      ((a11*b11+a12*b12, a11*b12+a12*b22),
       (a21*b11+a22*b21, a21*b12+a22*b22))

fib3 :: forall t a. (Integral t, Num a) => t -> a
fib3 = snd . snd . power a0 where a0 = (((0,1), (1,1)) :: Matrix2x2 a)
