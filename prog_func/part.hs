import Distribution.Simple.Utils (xargs)
import System.Posix.Internals (lstat)
import Control.Monad.RWS (MonadReader(ask))
import Data.Char (toUpper)
newtype All = All {getAll::Bool}
    deriving (Eq,Show)

instance Semigroup All where
    All a <> All b = All (a&&b)

instance Monoid All where
    mempty = All True

newtype Any = Any {getAny::Bool}
    deriving (Eq,Show)
instance Semigroup Any where
    Any a <> Any b = Any (a||b)

instance Monoid Any where
    mempty = Any False

newtype Sum a = Sum {getSum :: a}
    deriving (Eq,Show   )
instance Num a=>Semigroup (Sum a) where
    Sum a <> Sum b = Sum (a+b)

instance Num a => Monoid (Sum a) where
    mempty = Sum 0

newtype Product a = Product {getProduct :: a}
    deriving (Eq,Show)
instance Num a => Semigroup (Product a) where
    
    Product a <> Product b = Product (a*b)

instance Num a => Monoid (Product a) where
    mempty = Product 1

newtype Min a = Min {getMin :: a}
    deriving (Eq,Show)

instance Ord a => Semigroup (Min a) where
    Min a <> Min b = Min (min a b)

instance (Ord a,Bounded a)=> Monoid (Min a) where
    mempty = Min maxBound

newtype Max a = Max {getMax :: a}
    deriving (Eq,Show)

instance Ord a => Semigroup (Max a) where
    Max a <> Max b = Max (max a b)

instance (Ord a,Bounded a)=> Monoid (Max a) where

    mempty = Max minBound

data BinaryTree a =
    Leaf a
    | Node ( BinaryTree a ) ( BinaryTree a )
    deriving Show

foldTree :: ( a -> b -> b ) -> b -> BinaryTree a -> b
foldTree f i ( Leaf x ) = f x i
foldTree f i (Node l r ) = foldTree f ( foldTree f i r ) l

instance Foldable BinaryTree where
    foldr = foldTree


twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x<-xs
    if even x
        then [x*x,x*x]
        else [x*x]

selectOdd :: [Int]->[Int]
selectOdd xs = do
    x<-xs
    if odd x
        then [x]
        else []

getFromInterval :: Int -> Int -> [Int] -> [Int]
getFromInterval a b xs = do
    x<-xs
    if x>=a && x<=b
        then [x]
        else []

radical :: Float -> Maybe Float
radical x
    | x>=0 = return (sqrt x)
    | x<0 = Nothing

themselvesTimes :: [Int] -> [Int]
themselvesTimes ls = do
    l <- ls
    replicate l l

solEq2 :: Float -> Float -> Float -> Maybe Float
solEq2 0 0 0 = return 0
solEq2 0 0 c = Nothing
solEq2 0 b c = return ((negate c)/b)
solEq2 a b c = do
    rDelta <- radical (b*b-4*a*c)
    return ((negate b + rDelta) / (2 * a))


myLineDo :: IO String
myLineDo = do
    x <- getChar
    if x == '\n' then
        return []
    else do
        xs <- myLineDo
        return (x:xs)

echo :: IO ()
echo = do
    line <- getLine
    if line=="" then
        return ()
    else do
        putStrLn (map toUpper line)
        

myNumbers :: IO ()
myNumbers = do
    putStrLn ("Please enter a number: ")
    n <- readLn
    let m = n+1
    print m

simple :: IO ()
simple = do 
    x<- getChar
    putStrLn("\n")
    putChar x
    putStrLn("\n")

power :: Maybe Int -> Int -> Int
power Nothing n = 2^n
power (Just a) n = a^n

addInts :: [Either String Int] -> Int
addInts [] = 0
addInts (Left x : xs) = 0 + addInts xs
addInts (Right x : xs) = x + addInts xs
addInts2 :: [Either String Int]-> Int
addInts2 xs= sum[ x | Right x<-xs]