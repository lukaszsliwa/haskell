data Cyklista a = Elem a (Cyklista a) (Cyklista a)

newtype Cyc a b = Cyc (Cyklista a -> (b, Cyklista a))

instance Monad (Cyc a) where
	return a = Cyc (\m -> (a, m))
	(>>=) (Cyc cyc) f = 
		Cyc (\ s -> 
			let
				(x, s') = cyc s
				Cyc cyc' = f x
			in
				cyc' s')
