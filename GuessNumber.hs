module Main
	where

import IO
import Random

guessTheNumber num = do
	putStrLn "Let's guess the number: "
	number <- getLine
	case compare (read number) num of
		EQ -> putStrLn "Ok, you win!"
		LT -> do 
			putStrLn "Too low!"
			guessTheNumber num
		GT -> do
			putStrLn "Too high!"
			guessTheNumber num

main = do
	hSetBuffering stdin LineBuffering
	num <- randomRIO(1::Int, 100)
	putStrLn "Guess the number between 1 and 100"
	guessTheNumber num

