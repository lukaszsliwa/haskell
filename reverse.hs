showReverse :: [a] -> [a]
showReverse tab = rev tab [] where
	rev [] t = t
	rev (x:xs) y = rev xs (x:y)
