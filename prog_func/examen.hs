import Distribution.Simple.Utils (xargs)
import Control.Monad.Reader
newtype All = All { getAll :: Bool}
    deriving Show

instance Semigroup All where
    All x <> All y = All (x && y)

instance Monoid All where
    mempty = All True

newtype Any = Any { getAny :: Bool}
    deriving Show

instance Semigroup Any where
    Any x <> Any y = Any (x || y)

instance Monoid Any where
    mempty = Any False

newtype Sum a = Sum {getSum :: a}
    deriving (Eq,Show)
instance Num a => Semigroup (Sum a) where
    Sum a <> Sum b = Sum (a+b)

instance Num a => Monoid (Sum a) where
    mempty = Sum 0

newtype Product a = Product {getProduct :: a}
    deriving (Eq,Show)

instance Num a => Semigroup (Product a) where
    Product a <> Product b = Product (a*b)

instance Num a => Monoid (Product a) where
    mempty = Product 1

newtype Min a = Min { getMin :: a}
    deriving (Eq, Show)

instance Ord a => Semigroup (Min a) where
    Min a <> Min b = Min (min a b)

instance (Ord a,Bounded a) => Monoid (Min a) where
    mempty = Min maxBound

newtype Max a = Max {getMax :: a}
    deriving (Eq,Show)

instance Ord a => Semigroup (Max a) where
    Max a <> Max b = Max (max a b)

instance (Ord a,Bounded a) => Monoid (Max a) where
    mempty = Max minBound

data Point = Pt [Int]
    deriving Show

data Arb = Empty | Node Int Arb Arb
    deriving Show

class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a

instance ToFromArb Point where
    toArb (Pt []) = Empty
    toArb (Pt (x:xs))= Node x (toArb (Pt (filter (<x) xs))) (toArb (Pt (filter (>=x) xs)))
    
    fromArb Empty = Pt []
    fromArb (Node a b c)=let Pt l1 = fromArb b
                             Pt l2 = fromArb c
                         in Pt (l1 ++ [a] ++ l2)

getFromInterval :: Int -> Int -> [Int] -> [Int]
getFromInterval a b xs = [x|x<-xs,x>=a,x<=b]

getFromInterval2 :: Int -> Int -> [Int] -> [Int]
getFromInterval2 a b xs = do
    x<-xs
    if x>=a && x<=b
        then [x]
        else []

tom :: Reader String String
tom = do
    env <- ask
    return (env ++" This is Tom.")

jerry :: Reader String String
jerry = do
    env <- ask
    return (env ++" This is Jerry.")

tomAndJerry :: Reader String String
tomAndJerry = do
    t <- tom
    j <- jerry
    return (t++"\n"++j)


