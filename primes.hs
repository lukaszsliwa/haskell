primes :: Integer -> Integer -> [Integer]
primes start stop = [y | y <- [start..stop], (isPrime y) == False] where
	isPrime :: Integer -> Bool
	isPrime num = null [y | y <- [2..(num - 1)], (num `mod` y) == 0] == False
