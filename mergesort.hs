-- scala dwie posortowane listy
merge :: Ord a => [a] -> [a] -> [a]
merge [] b = b
merge a [] = a
merge a@(x:xs) b@(x':xs')
	| x < x' = x : (merge xs b)
	| otherwise = x' : (merge a xs')

-- sortuje dwie liczby w liscie
sortTwo :: Ord a => [a] -> [a]
sortTwo [] = []
sortTwo t@(a:b)
	| null t = []
	| null b = a : []
	| a < head b = a : b
	| otherwise = b ++ [a]

-- mergesort
mergesort :: Ord a => [a] -> [a]
mergesort tab = do
	let mid = (length tab) `div` 2
	if (length tab) <= 2
		then sortTwo tab
		else do
			let par = splitAt mid tab
			merge (mergesort (fst par)) (mergesort (snd par))
	
