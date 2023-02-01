ex1 :: [Int] -> Int
ex1 xs = foldr (+) 0 (map (^2) (filter odd xs))

ex2 :: [Bool] -> Bool
ex2 xs = foldr (&&) True xs

listToInt :: [Integer] -> Integer
listToInt xs = foldl (\x y->x*10+y) 0 xs

rmChar :: Char -> String -> String
rmChar c xs = filter (/=c) xs

rmCharsRec :: String -> String -> String
rmCharsRec a "" = ""
rmCharsRec "" b = b
rmCharsRec (x:xs) y = 
    if elem x y
        then rmCharsRec xs (rmChar x y)
        else rmCharsRec xs y

rmCharsFold :: String -> String -> String
rmCharsFold xs y = foldr rmChar y xs