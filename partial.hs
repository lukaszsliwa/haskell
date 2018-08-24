import Char

lcaseString :: [Char] -> [Char]
lcaseString = map toLower

lcaseConvString :: [Char] -> [Int]
lcaseConvString s = map (\s -> (fromEnum s) + 100) s

