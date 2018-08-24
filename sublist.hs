{- wygenerowac wszystkie k-elementowe listy takie, ze wszystkie
 - elementy w liscie nie maja wspolnego dzielnika -}

sublist t k = filter (\x -> length x == k) (sublist' t) where
	sublist' [] = [[]]
	sublist' (x:xs) = [ p | p <- sublist' xs ] ++ [ [x] ++ p | p <- sublist' xs ]
