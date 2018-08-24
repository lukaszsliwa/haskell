tail' :: [a] -> [a]
tail' [] = error "empty list"
tail' t = foldr f [] t where
	f x xs = if length' xs == (length' t) - 1 then xs
			else x : xs
				where
					length' = foldr (\ a b -> if b == 0 then 1 else b + 1) 0
