merge :: [Integer] -> [Integer] -> [Integer]
merge [] b = b
merge a [] = a
merge a@(x:xs) b@(x':xs')
	| x < x' = x : (merge xs b)
	| otherwise = x' : (merge a xs')

mergesort :: [Integer] -> [Integer]
mergesort tab = mergesortByIndex (length tab) tab where
	mergesortByIndex :: Int -> [Integer] -> [Integer]
	mergesortByIndex 0 _ = []
	mergesortByIndex 1 (a:_) = [a]
	mergesortByIndex 2 (a:b) = if a < head b then [a] ++ b else b ++ [a]
	mergesortByIndex len tab =
		merge (mergesort (take mid tab)) (mergesort (drop mid tab)) where
			mid = len `div` 2

