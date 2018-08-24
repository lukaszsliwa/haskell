
makeList :: [(Int, Int)] -> [Int]
makeList [] = []
makeList (x:xs) = replicate (snd x) (fst x)  ++ makeList xs

for :: (Int, Int)
for (start, end) = if start < end then for (start + 1, end)

kombinacje t = for (1, 10)
