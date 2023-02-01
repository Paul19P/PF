import Control.Monad.Writer
import Control.Monad.Reader
pos :: Int -> Bool
pos x = if(x>=0) then True else False

fct :: Maybe Int -> Maybe Bool
fct mx = mx >>= (\x -> Just(pos x))

fct2 :: Maybe Int -> Maybe Bool
fct2 mx = do
    x <- mx
    return (pos x)

addM :: Maybe Int -> Maybe Int -> Maybe Int
addM Nothing _ = Nothing
addM _ Nothing = Nothing
addM (Just a) (Just b) = Just (a+b)

addM2 :: Maybe Int -> Maybe Int -> Maybe Int
addM2 mx my = do
    x<-mx
    y<-my
    return (x+y)



cartesian_product xs ys = do
    x<-xs
    y<-ys
    return (x,y)

myGetLine :: IO String
myGetLine = do
    x<-getChar
    if x == '\n'  then
        return []
    else do
        xs<-myGetLine
        return (x:xs)

prelNo noin = sqrt noin

ioNumber :: IO ()
ioNumber = (readLn :: IO Float) >>= \noin-> putStrLn ("Intrare\n" ++ show noin) >> putStrLn "Iesire" >> print (prelNo noin)

half :: Int -> Writer String Int
half x = do
    tell ("I just halved "++(show x)++" !")
    return (x `div` 2)

newtype WriterS a = Writer { runWriter :: (a, String) }

