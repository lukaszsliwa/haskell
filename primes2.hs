primes :: Integer -> Integer -> [Integer]
primes start stop = [y | y <- [start..stop], (null [x | x <- [2..(y - 1)], (y `mod` x) == 0] == True)]
