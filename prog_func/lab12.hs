elem1 :: (Foldable t, Eq a) => a -> t a -> Bool
elem1 x xs = foldr (\a b->x == a || b) False xs

null1 :: (Foldable t) => t a -> Bool
null1 xs = foldr (\_ _ -> False) True xs