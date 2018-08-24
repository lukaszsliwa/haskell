import Monad

mo :: [Int] -> [Int]
mo n =
	do
		a <- n
		guard ( a == 0 )
		return 0

		--b <- n
		guard ( (a < 0) )
		return (-1)

		--c <- n
		guard ( (a > 0) )
		return 1

