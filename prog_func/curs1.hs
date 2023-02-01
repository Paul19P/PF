import Data.Char
h :: Int -> Int
h x | x==0 = 0
    | x==1 = y+1
    | x==2 = y*y
    | otherwise = y
    where y = x*x
x = let x = 3;y=6 in x*y

squares :: [Int] -> [Int]
squares [] = []
squares (x:xs) = x*x : squares xs

ords :: [Char] -> [Int]
ords xs = map ord xs

positives :: [Int] -> [Int]
positives xs = filter (>0) xs

digits :: [Char] -> [Char]
digits xs = filter isDigit xs

f::[Int]->Int
f = foldr (+) 0 .map (^2).filter (>0)

sums::[Int]->Int
sums xs = foldr (+) 0 xs

prod :: [Int] -> Int
prod xs = foldr (*) 1 xs

conc :: [String]->String
conc xs = foldr (++) [] xs

data Point a b = Pt a b

power :: Maybe Int->Int->Int
power Nothing n = 2^n
power (Just m) n =m^n

divide :: Int -> Int -> Maybe Int
divide n 0 = Nothing
divide n m = Just (n `div` m)

right :: Int -> Int -> Int
right n m = case divide n m of
            Nothing -> 3
            Just r -> r+3

mylist :: [ Either Int String ]
mylist = [ Left 4 , Left 1 , Right "hello" , Left 2 ,Right " " , Right "world" , Left 17]

addints :: [Either Int String] -> Int
addints [] = 0
addints (Left x:xs) = x+addints xs
addints (Right x:xs) = addints xs

addstrs :: [Either Int String] -> String
addstrs [] = []
addstrs (Right x:xs) = x++addstrs xs
addstrs (Left x:xs) = addstrs xs

addstrs' :: [Either Int String] -> String
addstrs' xs = concat[x|Right x<-xs]

addints' :: [Either Int String] -> Int
addints' xs = sum[x| Left x<-xs]