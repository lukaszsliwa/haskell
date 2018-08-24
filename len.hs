my :: [Integer] -> Integer
my t = if t == [] then 0 else 1 + (my . tail) t

