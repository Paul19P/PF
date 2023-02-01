poly2 :: Double -> Double -> Double ->Double -> Double
poly2 a b c x = a*x*x+b*x+c

eeny :: Integer -> String
eeny x = 
        if x `mod` 2==0
            then "eeny"
            else "meeny"
        
fizzbuzz :: Integer -> String
fizzbuzz x
    | x `mod` 15 ==0 = "FizzBuzz"
    | x `mod` 3 ==0 ="Fizz"
    | x `mod` 5 ==0 ="Buzz"

tribonacci :: Integer -> Integer
tribonacci 1 = 1
tribonacci 2 = 1
tribonacci 3 = 2
tribonacci n = tribonacci(n-1)+tribonacci(n-2)+tribonacci(n-3)

binomial :: Integer -> Integer -> Integer
binomial n 0 = 1
binomial 0 k = 0
binomial n k = binomial (n-1) k + binomial (n-1) (k-1)

verifL :: [Int] -> Bool
verifL x = if even (length x)
                then True
                else False

takefinal :: [Char] -> Int -> [Char]
takefinal x n = if (length x)<n
                    then x
                    else take n (reverse x)

remove :: [Int] -> Int -> [Int]
remove x n = take (n-1) x ++ drop n x   

sumImp :: [Int] -> Int
sumImp [] =0
sumImp (x:xs)
    | even x = 0+sumImp xs
    | odd x = x+sumImp xs

totalLen :: [String] ->Int
totalLen [] = 0
totalLen (x:xs)
    | take 1 x=="A" = length x + totalLen xs
    | otherwise = 0 + totalLen xs