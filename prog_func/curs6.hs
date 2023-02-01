my_elem :: Eq a => a -> [a] -> Bool
my_elem x ys = foldr (||) False (map (\y->x==y) ys)

data Point = Pt Double Double
            deriving (Eq,Show)

type Nume=String
type Prenume=String
data Person = Person Nume Prenume
x :: Person -> String
x (Person nume _) = nume 
y :: Person -> Prenume
y (Person _ prenume) = prenume

f ::(Int,String) -> String
f(n,s) = take n s

foo :: ((Int,Char),String) -> String
foo = undefined

fx::[Int]->Int
fx = foldr (+) 0.map (^2).filter (>0)