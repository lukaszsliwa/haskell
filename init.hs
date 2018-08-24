init' :: [a] -> [a]
init' t = rev (foldl f [] t) where
		rev t = foldl (\ a b -> b : a) [] t
		f xs x = if length' xs == (length' t) - 1
				then xs
				else x : xs
					where
						length' = foldr (\ a b -> if b == 0 then 1 else b + 1) 0
