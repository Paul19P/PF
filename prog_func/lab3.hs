import Data.Char (isDigit, digitToInt)
nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale (x:xs)
    | reverse x==x = length (filter (\x -> elem x "aeiou") x)+nrVocale xs
    | otherwise = 0 + nrVocale xs

f :: Int -> [Int] -> [Int]
f n [] = []
f n (x:xs)
    | even x = [x] ++ [n] ++ (f n xs)
    | otherwise = [x] ++ (f n xs)

divizori :: Int -> [Int]
divizori n = [x|x<-[1..n],n `mod` x ==0]

listadiv :: [Int] -> [[Int]]
listadiv xs = [divizori x | x<-xs ]

inInterval :: Int -> Int -> [Int] -> [Int]
inInterval a b xs=[x|x<-xs,x>=a && x<=b]

inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec a b [] = []
inIntervalRec a b (x:xs)
    | x>=a && x<=b = [x]++inIntervalRec a b xs
    | otherwise = inIntervalRec a b xs

pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (x:xs)
    | x>0 = 1+pozitiveRec xs
    | otherwise = 0+pozitiveRec xs

pozitiveComp :: [Int] -> Int
pozitiveComp xs= length [x|x<-xs,x>0]

multDigits :: String -> Int
multDigits "" = 1
multDigits (x:xs)
    | isDigit x = digitToInt x*multDigits xs
    | otherwise = 1*multDigits xs

multDigits2 :: String -> Int
multDigits2 xs =product [digitToInt x|x<-xs,isDigit x]