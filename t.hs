t :: [a] -> [a] -> ([a], [a])
t a b =	( [ x | x <- a, y <- b] , [ x | y <- a, x <- b ] )
