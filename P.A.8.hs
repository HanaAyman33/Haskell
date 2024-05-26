--Excercise 1
maxList :: (Ord a,Num a) => [a] -> a
maxList [] = 0
maxList (a:x) = if maxList x < a 
            then a else maxList x

--Excercise 2
data List a = Null | Cons a (List a)
len :: List a -> Int
len Null = 0
len (Cons a l) = 1 + len l

--Excercise 3
data Tree = Nil | BT Int Tree Tree deriving Show
sumTree :: Tree -> Int
sumTree Nil = 0
sumTree (BT cur left right) = cur + sumTree left + sumTree right

doubleTree :: Tree -> Tree
doubleTree Nil = Nil
doubleTree (BT node l r) = BT (2*node) (doubleTree l) (doubleTree r)

maxElement :: Tree -> Int
maxElement Nil = 0
maxElement (BT node r l) = maxHelper (maxHelper node l) r
maxHelper :: Int -> Tree -> Int
maxHelper node (BT curr r l) = if(node>curr) 
        then maxHelper (maxHelper node l) r
        else maxHelper (maxHelper curr l) r

--Excercise 4
data Expr = N Int
            |Plus Expr Expr
            |Equal Expr Expr
            |Not Expr
            deriving Show
data Val = I Int
           |B Bool
        deriving Show
eval ::  Expr -> Val
eval (N i) = (I i)
eval (Plus e1 e2) = I (i+j) where (I i,I j) = (eval e1,eval e2)
eval (Equal e1 e2) = B (i==j) where (I i,I j) = (eval e1,eval e2)
eval (Not e) = B (not b) where (B b) = eval e
--eval (Plus (N x) (N y)) = (I (x+y))
--eval (Equal (Plus (N x1) (N y1)) (Plus (N x2) (N y2))) 
 --   = if(x1+y1==x2+y2) then (B True) else (B False)
--eval (Not x) = B (not b) where (B b) = eval x

--Excercise 5
data Entry = E String [Int] String deriving (Show,Eq)
--the "C" is optional
data ContactsBook = C [Entry]

getInfo :: String -> ContactsBook -> (String,[Int])
getInfo name (C []) = error "This name does not exist"
getInfo name (C ((E n phone email):rest)) 
    = if(name==n) then (name,phone)
                  else getInfo name (C rest)
addContact :: Entry -> ContactsBook -> ContactsBook
addContact (E name phone email) (C ((E n p e):rest)) 
    = if(name<=n) then C ((E name phone email):(E n p e):rest)
        else addContact (E name phone email) (C rest)