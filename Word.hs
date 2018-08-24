module Main
	where

import IO

askForWords = do
	putStrLn "Podaj slowo: "
	word <- getLine
	if word == ""
		then return []
		else do
			r <- askForWords
			return (word : r)

main = do
	hSetBuffering stdin LineBuffering
	askForWords
