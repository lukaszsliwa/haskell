import Control.Monad

type Generator = []
type Rozmiar = Int
type Ustawienie = [Int]

--abs :: Int -> Int
--abs e | e < 0 = (-e) | otherwise =  e

szachuje :: Int -> Ustawienie -> Bool
szachuje wi tab = wi `elem` tab

przekatna wiersz wj kolumna j = (wj == wiersz || abs(j - kolumna) == abs(wj - wiersz))

szachujePrzekatne :: Int -> Int -> Ustawienie -> Bool
szachujePrzekatne wiersz kolumna tab = 
		not ( null ( [ k | k <- [1..(kolumna-1)], przekatna wiersz (tab!!k + 1) kolumna k ] ) )

wolneWiersze :: Int -> Rozmiar -> Ustawienie -> Ustawienie
wolneWiersze kolumna rozmiar tab = 
	do
		wiersz <- [1..rozmiar]
		guard ( not ( szachuje wiersz tab ) && not ( szachujePrzekatne wiersz kolumna tab  ) )
		return (wiersz)
	

hetmany :: Rozmiar -> Generator Ustawienie
hetmany rozmiar = hetmany' 1 rozmiar [] where
	hetmany' :: Int -> Rozmiar -> [Ustawienie] -> [Ustawienie]
	hetmany' kolumna rozmiar tab
		| kolumna > rozmiar = tab
		| otherwise =  ustawiaj (wolneWiersze kolumna rozmiar tab) where
			ustawiaj (w:ws) = hetmany' (kolumna + 1) rozmiar (tab ++ [w]) ++ ustawiaj ws
