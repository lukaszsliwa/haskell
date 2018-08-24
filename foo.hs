f :: Num a => a -> a
f n = 2 * n

g :: Num a => a -> a
g n = (-n)

h :: Num a => a -> a
h n = g . f $ n

