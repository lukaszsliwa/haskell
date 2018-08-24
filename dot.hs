f :: Integer -> Integer
f n = 2 * n

g :: (Integer -> Integer) -> Integer -> Integer
g ff m = ff m

fun' :: Integer -> Integer -> Integer
fun' = (+)
