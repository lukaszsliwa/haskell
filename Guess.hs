module Main
	where

import IO

main = do
  hSetBuffering stdin LineBuffering
  putStrLn "Podaj imie: "
  name <- getLine
  putStrLn ("Witaj " ++ name ++ "!")
