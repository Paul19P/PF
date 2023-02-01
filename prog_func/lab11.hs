data List a = Nil
    | Cons a (List a)
    deriving (Eq, Show)

instance Functor List where
    fmap :: (a -> b) -> List a -> List b
    fmap _ Nil = Nil
    fmap f (Cons a b)=Cons (f a)(fmap f b)

app :: List a -> List a -> List a
app Nil b = b
app (Cons h t) b = Cons h (app t b)

instance Applicative List where
    pure a = Cons a Nil
    a <*> Nil = Nil
    Nil <*> a = Nil
    (Cons a b) <*> c =  app (fmap a c) (b <*> c)

data Cow = Cow {
    name :: String,
    age :: Int,
    weight :: Int
} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty ""=Nothing
noEmpty a = Just a

noNegative :: Int -> Maybe Int
noNegative x
    | x<0 = Nothing
    | otherwise = Just x

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString nume varsta greutate
    | noEmpty nume == Nothing = Nothing
    | noNegative varsta == Nothing = Nothing
    | noNegative greutate == Nothing = Nothing
    | otherwise = Just (Cow nume varsta greutate)

newtype Name = Name String 
    deriving (Eq, Show)
newtype Address = Address String 
    deriving (Eq, Show)
data Person = Person Name Address
    deriving (Eq, Show)

validateLength :: Int -> String -> Maybe String
validateLength a b 
    | (length b)<a = Just b
    | otherwise = Nothing

mkName :: String -> Maybe Name
mkName a
    | validateLength 25 a == Nothing = Nothing
    | otherwise = Just (Name a)

mkAddress :: String -> Maybe Address
mkAddress a
    | validateLength 100 a == Nothing = Nothing
    | otherwise = Just (Address a)
