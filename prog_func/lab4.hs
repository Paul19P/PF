factori :: Int -> [Int]
factori n = [x|x<-[1..n],n `mod` x==0]

prim :: Int -> Bool
prim n = if length (factori n)==2
            then True
            else False
numerePrime :: Int -> [Int]
numerePrime n = [x|x<-[2..n],prim x]

myzip3 :: [Int]->[Int]->[Int]->[(Int,Int,Int)]
myzip3 (x:xs) (y:ys) (z:zs) = [(x,y,z)]++myzip3 xs ys zs
myzip3 _ _ _ = []

firstEl :: [(a,b)] ->[a]
firstEl xs = map fst xs

sumList :: [[Int]]->[Int]
sumList xs = map (\x -> sum x) xs

pre12 :: [Int]->[Int]
pre12 xs = map (\x-> if even x then x `div` 2 else x*2) xs

ex8 :: Char -> [String] -> [String]
ex8 c xs = filter (\x->elem c x) xs

ex9 :: [Int] -> [Int]
ex9 xs = map (^2) (filter odd xs)

ex10 :: [Int] -> [Int]
ex10 xs =map (\(a,b)->a*a) (filter (odd.snd) (zip xs [1..(length xs)]))


mymap :: (a->b)->[a]->[b]
mymap f [] = []
mymap f (x:xs) = f x : mymap f xs

myfilter :: (a->Bool)->[a]->[a]
myfilter f [] =[]
myfilter f (x:xs)=
    if f x 
        then x:myfilter f xs
        else myfilter f xs
    