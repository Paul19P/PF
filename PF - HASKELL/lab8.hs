import System.Win32 (xBUTTON1)
data Punct = Pt [Int]

data Arb = Vid | F Int | N Arb Arb
          deriving Show

class ToFromArb a where
 	    toArb :: a -> Arb
	    fromArb :: Arb -> a

instance Show Punct where
  show (Pt l) = "(" ++ parse l ++ ")"
    where
      parse [] = ""
      parse [x] = show x
      parse (h:t) = show h ++ "," ++ parse t
-- Pt [1,2,3]
-- (1, 2, 3)

-- Pt []
-- ()

instance ToFromArb Punct where
  toArb (Pt []) = Vid
  toArb (Pt (h:t)) = N (F h) (toArb (Pt t))
  fromArb Vid = Pt []
  fromArb (F x) = Pt [x]
  fromArb (N x y) = Pt (l1 ++ l2)
    where
      Pt l1 = fromArb x
      Pt l2 = fromArb y
-- toArb (Pt [1,2,3])
-- N (F 1) (N (F 2) (N (F 3) Vid))
-- fromArb $ N (F 1) (N (F 2) (N (F 3) Vid)) :: Punct
--  (1,2,3)
data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

class GeoOps g where
  perimeter :: (Floating a) => g a -> a
  area :: (Floating a) =>  g a -> a

-- ghci> pi
-- 3.141592653589793

instance GeoOps Geo where
  perimeter (Square a) = 4 * a
  perimeter (Rectangle a b) = 2 * a + 2 * b
  perimeter (Circle a) = 2 * pi * a
  area (Square a) = a * a
  area (Rectangle a b) = a * b
  area (Circle a) = pi * (a * a)

instance (Floating a, Eq a) => Eq (Geo a) where
  -- (==) :: a -> a -> Bool
  (==) :: Geo a -> Geo a -> Bool
  (==) a b = perimeter a == perimeter b