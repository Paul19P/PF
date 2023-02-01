import Data.List
import Prelude
import Distribution.Utils.MD5 (binaryGetMD5)
import System.Win32 (xBUTTON1, COORD (yPos))
import Data.Array
import Data.Char (digitToInt)
import Data.Char
import GHC.Float (powerDouble)
import System.FilePath (combine)

sumRoot :: Integer -> Integer -> Integer
sumRoot x y = x*x + y*y

parity x = if (mod x 2 == 0)
                then "eeny"
                else "meeny"

factorial :: (Num a, Enum a) => a -> a
factorial x = product [1..x]

func :: (Ord a, Num a) => a -> a -> Bool
func x y= if (x > 2*y)
                then True
                else False

poly2 :: Double -> Double -> Double -> Double -> Double
poly2 a b c x = a*x*x+b*x+c

fiboCazuri n
    | n == 1 || n ==2   = 1
    | n == 3            = 2
    | otherwise         = fiboCazuri (n-1) + fiboCazuri (n-2) + fiboCazuri(n-3)

binomial (x,y)
    | y == 0    = 1
    | x == 0    = 0
    | otherwise = binomial(x-1,y) + binomial(x-1,y-1)

verifL x
    | mod (length x) 2 == 0     = True
    | otherwise                 = False

takefinal x n
    | length x < n  = x
    | otherwise     = drop (length x-n) x


remove x n
    | length x < n  = []
    | otherwise     = drop (length(take n x)) (take (n+1) x)

-- arrayRec :: Integer -> Integer -> [Integer]
-- arrayRec [] = []
-- arrayRec (x:v)
--     | x > 1     = v : t'
--     | otherwise = t'
-- where

myReplicate:: Integer ->Integer -> [Integer]
myReplicate 0 v = []
myReplicate n v =  v:myReplicate (n-1) v

sumImp :: [Integer] -> Integer
sumImp [] = 0
sumImp (h:t)
    |odd h      = h + sumImp t
    |otherwise  = sumImp t

totalLen :: [String] -> Int
totalLen [] = 0
totalLen (h:t)
    | head h == 'A'      = length h + totalLen t
    | otherwise          = totalLen t

nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale (h:t)
    | h == reverse h    = countVocale h + nrVocale t
    | otherwise     = nrVocale t

countVocale :: String -> Int
countVocale [] = 0
countVocale (h:t)
    | elem h "aeiouAEIOU"   = 1 + countVocale t 
    | otherwise             = countVocale t

f :: Int -> [Int] -> [Int]
f n [] = []
f n (h:t)
    | even h    = h : n : f n t
    | otherwise = h : f n t

divizori :: Int -> [Int]
divizori n = [ x | x <- [1..n] , mod n x == 0]

listadiv :: [Int] -> [[Int]]
listadiv [] = []
listadiv (h:t) = divizori h : listadiv t

inInterval :: Int -> Int -> [Int] -> Int
inInterval n x [] = 0
inInterval n x (h:t)
    | h >= n && h <= x          = h + inInterval n x t 
    | otherwise                 = inInterval n x t

inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp x y z = [ c | c <- z, c >= x, c <= y]

pozitive :: [Int] -> Int
pozitive [] = 0
pozitive (h:t)
    | h > 0     = 1 + pozitive t 
    | otherwise = pozitive t

pozitiveComp :: [Int] -> Int
pozitiveComp x = length [ c | c <- x, c > 0]

pozitiiImpare :: [Int] -> Int -> [Int]
pozitiiImpare [] d = []
pozitiiImpare (h:t) d
    |odd h     = d : pozitiiImpare t (d+1)
    |otherwise = pozitiiImpare t (d+1)

pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp x = [ i | (i,v) <- zip [0..] x, odd(v)]

multDigitsRec :: [Char] -> Int
multDigitsRec [] = 1
multDigitsRec (h:t)
    | isDigit h     = digitToInt h * multDigitsRec t
    | otherwise     = multDigitsRec t

multDigitsComp :: [Char] -> Int
multDigitsComp x = product [digitToInt c | c <- x, isDigit c]

factori :: Int -> [Int]
factori n = [ c | c <-[1..n], mod n c == 0]

prim :: Int -> Bool
prim n = length [ c | c <-[1..n], mod n c == 0] == 2

numerePrime :: Int -> [Int]
numerePrime x = [ c | c <- [2..x], prim c]

myzip3 :: [Int] -> [Int] -> [Int] -> [(Int,Int,Int)]
myzip3 [] x y = []
myzip3  x [] y = []
myzip3  x y []= []
myzip3 (h:t) (h2:t2) (h3:t3) = (h,h2,h3) : myzip3 t t2 t3

myzip3Comp :: [a] -> [b] -> [c] -> [(a,b,c)]
myzip3Comp x y z = [ (a,b,c) | ((a,b),c) <- zip (zip x y) z]

firstEl :: [(b,c)] -> [b]
firstEl  = map fst

sumList :: (Foldable t, Num b) => [t b] -> [b]
sumList  = map sum

pre12 :: [Int] -> [Int]
pre12 x = map (\x -> if even x then x `div` 2 else x*2) x


functContain :: Char -> [String] -> [String]
functContain x = filter (elem x)

fct9 :: [Int] -> [Int]
fct9 x = map (\x -> x*x) (filter odd x)

fct10 :: [Int] -> [Int]
fct10 x = map (\(a,b) -> b*b) (filter pozImp2 (zip [0..] x))

pozImp2 :: Integral a => (a, b) -> Bool
pozImp2 (a,b)
    | odd a     = True
    | otherwise = False

fct11 x = mymap vocale x

vocale :: String -> String
vocale [] = []
vocale (h:t)
    | elem h "aeiouAEIOU"   = h : vocale t 
    | otherwise             = vocale t

mymap :: (a -> b) -> [a] -> [b]
mymap x [] = []
mymap x (h:t) = x h : mymap x t

myfilter :: (t1 -> Bool) -> [t1] -> [t1]
myfilter x [] = []
myfilter x (h:t)
    | x h == True       = h : myfilter x t
    | otherwise         = myfilter x t

lab5ex1 :: Num b => [b] -> b
lab5ex1 x = foldr (+) 0 (map (\(a,b) -> b*b) (filter pozImp2 (zip [0..] x)))

lab5ex2 :: Foldable t => t Bool -> Bool
lab5ex2 x = foldr (==) True x

allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies y x = lab5ex2 (mapFoldr y x)

anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies y x = elem True (mapFoldr y x)


filterFoldr x y = lab5ex2 (map x y)

mapFoldr f = foldr (\x xs -> f x : xs) []


listToInt x = foldl (\c digit -> c*10 + digit) 0 x


rmChar :: Char -> String -> String
rmChar x [] = []
rmChar x (h:t)
    | x == h        = rmChar x t
    | otherwise     = h : rmChar x t

rmCharRecursiv :: String -> String -> String
rmCharRecursiv [] y = y
rmCharRecursiv (h:t) y = rmCharRecursiv t (rmChar h y)

-- rmCharFold :: String -> String -> String
-- rmCharFold x y = foldr (==) False elem x y

data List a = Nil | Cons a (List a)
    deriving Show

data Either a b = Lef a | Rig b
    deriving Show

data Meal = Breakfast | Lunch | Dinner
    deriving Show

abcd :: Int -> Char -> Char
abcd e c = 'a'

-- data Tip a = Tip a a
-- faaa :: Num a => Tip a -> Tip b -> a
-- faaa (Tip x1 y1) (Tip x2 y2) = x1 + y1 + x2 + y2

-- l1 = [2,4..]
-- l2 = [10,20..]
-- l3 = zip l1 l2
-- l4 = head tail l3