insertsort :: [a] -> [a]
insertsort tab@(x:xs) = isort tab 0 (length tab) where
	isort (x:xs) start stop
		| (start + 1 == stop) = (x:xs)
		| otherwise = isort (swapWhileR (<) (x:xs)) (start + 1) stop

swapWhileR :: (a -> a -> Bool) -> [a] -> [a]
swapWhileR f tab = reverse $ swapWhileR2 f $ reverse tab where
	swapWhileR2 f (x:xs)
		| x (f) (head xs) == True = (head xs) : swapWhileR2 f (x : (tail xs))
		| otherwise = (x:xs)
		
