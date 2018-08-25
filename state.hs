data Tree = Node Tree Int Tree | Leaf deriving Show

-- Wersja „tradycyjna”

renumber :: Tree -> Tree
renumber = fst . aux 0 where
   aux n Leaf = (Leaf,n)
   aux n (Node l _ r) =
      let
         (l',n') = aux n l
         (r',n'') = aux (n'+1) r
      in (Node l' n' r', n'')

-- Ukrywanie licznika

newtype IntState a = IntState (Int -> (a,Int))

instance Monad IntState where
   (IntState st1) >>= f =
      IntState (\ s -> let
                          (x,s') = st1 s
                          IntState st2 = f x
                       in st2 s')
   return x = IntState (\ s -> (x,s))

fetch :: IntState Int
fetch = IntState (\ s -> (s,s))

assign :: Int -> IntState ()
assign s' = IntState (\ _ -> ((),s'))

run :: Int -> IntState a -> a
run s (IntState st) = fst (st s)

-- Por. state.sml — imperatywny
renumber' :: Tree -> Tree
renumber' t = run 0 (aux t) where
   aux Leaf = return Leaf
   aux (Node l _ r) = aux l >>=
      \ l' -> genNumber >>=
      \ n  -> aux r >>=
      \ r' -> return (Node l' n r') where
   genNumber = fetch >>=
      \ n -> assign (n+1) >>
      return n

-- wersja z do-notacją
renumber'' :: Tree -> Tree
renumber'' t = run 0 (aux t) where
   aux Leaf = return Leaf
   aux (Node l _ r) = do
      l' <- aux l
      n <- genNumber
      r' <- aux r
      return (Node l' n r') where
   genNumber = do
      n <- fetch
      assign (n+1)
      return n

