{- 
class Functor f where 
     fmap :: (a -> b) -> f a -> f b 
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

Just length <*> Just "world"

Just (++" world") <*> Just "hello,"
pure (+) <*> Just 3 <*> Just 5
pure (+) <*> Just 3 <*> Nothing
(++) <$> ["ha","heh"] <*> ["?","!"]
-}
data List a = Nil
            | Cons a (List a)
        deriving (Eq, Show)

instance Functor List where
    -- fmap :: (a -> b) -> List a -> List b
    fmap a Nil = Nil
    fmap a (Cons x y) = (Cons (a x) (fmap a y))
app :: List a -> List a -> List a
app Nil b = b
app (Cons h t) b = Cons h (app t b)
instance Applicative List where
    pure a = Cons a Nil
    (<*>) (Cons h t) Nil = Nil
    (<*>) (Cons h t) a = app (fmap h a) (t <*> a)

f = Cons (+1) (Cons (*2) Nil)
v = Cons 1 (Cons 2 Nil)
test1 = (f <*> v) == Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))

data Cow = Cow {
        name :: String
        , age :: Int
        , weight :: Int
        } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty a = Just a

noNegative :: Int -> Maybe Int
noNegative a
    | a < 0     = Nothing
    | otherwise = Just a

test21 = noEmpty "abc" == Just "abc"
test22 = noNegative (-5) == Nothing 
test23 = noNegative 5 == Just 5 

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString nume varsta greutate
    | noEmpty nume == Nothing       = Nothing
    | noNegative varsta == Nothing  = Nothing
    | noNegative greutate == Nothing= Nothing
    | otherwise                     = Just (Cow nume varsta greutate)

test24 = cowFromString "Milka" 5 100 == Just (Cow {name = "Milka", age = 5, weight = 100})

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person Name Address	
    deriving (Eq, Show)

validateLength :: Int -> String -> Maybe String
validateLength a b
    | length b < a      = Just b
    | otherwise          = Nothing

test31 = validateLength 5 "abc" == Just "abc"
mkName :: String -> Maybe Name
mkName a
    | validateLength 25 a == Nothing    = Nothing
    | otherwise                         = Just (Name a)

mkAddress :: String -> Maybe Address
mkAddress a
    | validateLength 100 a == Nothing    = Nothing
    | otherwise                         = Just (Address a)

test32 = mkName "Gigel" ==  Just (Name "Gigel")
test33 = mkAddress "Str Academiei" ==  Just (Address "Str Academiei")

mkPerson :: String -> String -> Maybe Person
mkPerson a b
    | mkName a == Nothing     = Nothing
    | mkAddress b == Nothing  = Nothing
    | otherwise               = Just (Person (Name a) (Address b))

test34 = mkPerson "Gigel" "Str Academiei" == Just (Person (Name "Gigel") (Address "Str Academiei"))

-- instance Functor Person where
--     fmap f (Person (Nothing) b) = Nothing