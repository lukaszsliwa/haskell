--type Komorka = (Int, Int)
data Para a = Pusta | Pelna (a, a)

emptyPair :: Para Int -> Para Int -> Para Int
emptyPair Pusta b = b
emptyPair a b = a
