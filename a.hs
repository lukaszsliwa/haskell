test :: [Integer]
test = 
	let 
		e' = [1..3]
	in
		[ e | p <- e' ]
