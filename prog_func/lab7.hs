data Expr = Const Int -- integer constant
    | Expr :+: Expr -- addition
    | Expr :*: Expr -- multiplication
    deriving Eq

data Operation = Add | Mult 
    deriving (Eq, Show)

data Tree = Lf Int -- leaf
    | Node Operation Tree Tree -- branch
    deriving (Eq, Show)

instance Show Expr where
    show (Const a)=show a
    show (a :+: b) = "(" ++ show a ++ "+" ++ show b ++ ")"
    show (a:*:b)=show a ++" * "++show b

evalExp :: Expr -> Int
evalExp (Const a)=a
evalExp (a:+:b)=evalExp a+evalExp b
evalExp (a:*:b)=evalExp a*evalExp b

evalArb :: Tree -> Int
evalArb (Lf a)=a
evalArb (Node Add a b)=evalArb a + evalArb b
evalArb (Node Mult a b)=evalArb a * evalArb b

expToArb :: Expr ->Tree
expToArb (Const a :+: Const b)=Node Add (Lf a) (Lf b)
expToArb (Const a :*: Const b)=Node Mult (Lf a) (Lf b)
expToArb (a:+:b) = Node Add (expToArb a) (expToArb b)
expToArb (a:*:b) = Node Mult (expToArb a) (expToArb b)