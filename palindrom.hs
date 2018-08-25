palindrom :: IO ()
palindrom = 
  do putStr "Wprowadz napis: "
     str <- getLine
     if (str == reverse str)
        then putStrLn "Palindrom."
	else putStrLn "To nie jest palindrom."

