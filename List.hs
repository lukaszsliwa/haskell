data List a = Nil | Cons a (List a)

listLength :: (List a) -> Integer
listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs

listPush :: (List a) -> a -> (List a)
listPush Nil e = Cons e Nil
listPush (Cons x xs) el = Cons x (listPush xs el)
