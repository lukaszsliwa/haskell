ostatni :: [a] -> a
-- ostatni (_:x:[]) = x
ostatni [x] = x
ostatni (x:xs) = ostatni xs
