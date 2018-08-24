charByChar :: String -> [Char]
charByChar str = do
	if tail str == []
		then (head str) : []
		else (head str) : charByChar (tail str)
