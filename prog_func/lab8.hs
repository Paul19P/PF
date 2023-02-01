import Distribution.Simple.Utils (xargs)
data Punct = Pt [Int]

data Arb = Vid | F Int | N Arb Arb
    deriving Show

class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a

instance Show Punct where
    show (Pt l)="("++parse l++")"
        where
            parse [] = ""
            parse [x]=show x
            parse (x:xs)=show x ++ "," ++ parse xs 

instance ToFromArb Punct where
    toArb (Pt [])=Vid
    toArb (Pt (x:xs))= N (F x) (toArb (Pt xs))
    fromArb Vid = Pt []
    fromArb (F x) = Pt [x]
    fromArb (N x y)=Pt (l1++l2)
        where 
            Pt l1 = fromArb x
            Pt l2 = fromArb y

data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

class GeoOps g where
    perimeter :: (Floating a) => g a -> a
    area :: (Floating a) => g a -> a

instance GeoOps Geo where
    perimeter (Square a)=4*a
    perimeter (Rectangle a b)=2*a + 2*b
    perimeter (Circle a)=2*pi*a
    area (Square a)=a*a
    area (Rectangle a b)=a*b
    area (Circle a)=pi*a*a

instance (Eq a,Floating a)=> Eq (Geo a) where
    (==) :: Geo a -> Geo a -> Bool
    a==b = perimeter a == perimeter b






