merge :: [Integer] -> [Integer] -> [Integer]
merge [] b = b
merge a [] = a
merge a@(x:xs) b@(x':xs')
	| x < x' = x : (merge xs b)
	| otherwise = x' : (merge a xs')
	
