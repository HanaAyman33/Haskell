--Excercise 1
square :: Num a=> a -> a
square x = x*x

cube :: Num a=> a -> a
cube x = x*x*x

cube2 :: Num a=> a -> a
cube2 x = x*(square x)

fourthpower :: Num a=> a -> a
fourthpower x = square (square x)

isTriple :: (Eq a,Num a)=> a-> a-> a-> Bool
isTriple x y z = if square z == square x 
        + square y then True else False

-------------------------------------------------
--Excercise 2

threeDifferent :: (Eq a,Num a) => a-> a-> a-> Bool
threeDifferent m n p = if 
    n==p || m==p || n==m then False else True
threeDifferent2 m n p  | m==n || n==p || p==n = False
                       | otherwise = True

-------------------------------------------------
--Excercise 3

smaller :: (Ord a,Num a) => a-> a-> a
smaller x y
          | x <= y = x
          | otherwise = y

minThree :: (Ord a,Num a) => a-> a-> a-> a
minThree x y z  | x<=y && x<=z = x
                | y<=x && y<=z = y
                | otherwise = z

minThree2 :: (Ord a,Num a) => a-> a-> a-> a
minThree2 x y z = smaller x (smaller y z)

-------------------------------------------------
--Excercise 4

sumsq :: (Eq a,Num a) => a -> a
sumsq 1 = 1
sumsq x = (x*x) + sumsq (x-1)

fib :: (Eq a, Num a) => a-> a
fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

isPrime :: Integral t => t -> Bool
--isPrime :: Int->Bool
isPrime 2 = True
isPrime 3 = True
isPrime x = if hasNoFactor x (div x 2) then True else False

hasNoFactor x 2 = mod x 2/=0
hasNoFactor x n = if mod x n ==0 then False 
                else hasNoFactor x (n-1)

primeGEQ x =  if isPrime (x) then x else primeGEQ (x+1)

--IMPORTANT
gcd1 :: Integral a=> a->a-> a
gcd1 a b = if a<b then gcd1 b a 
            else if b==0 then a 
                else gcd1 b (mod a b)

--power :: Integral a=> a->a-> a
power :: (Num t2, Integral t1) => t2 -> t1 -> t2
power a 0 = 1
power a 1 = a
power a b = if even b then square (power a (div b 2))
            else a * power a (b-1)

-------------------------------------------------

--Excercise 5

last1 :: [a] -> a
last1 [] = error "the list is empty"
last1 [h] = h
last1 (h:t) = last1 t

occursIn :: Eq t => t -> [t] -> Bool
--occursIn x xs = elem x xs
occursIn x [] = False
occursIn x (h:t) = if x==h then True else occursIn x t 

occurs :: Eq t => t -> [[t]] -> [[t]]
occurs a [] = []
occurs x (h:t) = if occursIn x h 
        then (h:(occurs x t))
            else occurs x t

reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (h:t) = reverse1 t ++ [h]
--Another solution
reverse2 [] = []
reverse2 l = last1 l : reverse2 (init l)

--IMPORTANT--
maxList :: (Num a, Ord a) => [a] -> a
maxList [] = 0
maxList (x:xs)  | maxList xs > x = maxList xs
                |otherwise  = x

updatePrices :: Fractional t => t -> [(a, t)] -> [(a, t)]
updatePrices _ [] = []
updatePrices p ((str,x):t) =  
        (str,x+x*(p/100)):updatePrices p t
 
--------------------------------------------------

--Excercise 6

palindrome :: String -> Bool
palindrome str  = reverse str == str

--------------------------------------------------

--Excercise 7

--prefix :: Eq a => [a] -> [a]-> Bool
prefix [] y = True
prefix (x:xs) (y:ys) = if(x==y) 
            then prefix xs ys   
                else False    

--------------------------------------------------

--Excercise 8

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (h:t) = if(x<=h) 
        then [x]++(h:t)  -- or instead write =>> (x:h:t) 
            else (h:insert x t)

sort1 :: Ord a => [a] -> [a]
sort1 [] = []
sort1 (x:xs) = insert x (sort1 xs)

--------------------------------------------------

--Excercise 9

merge :: Ord a => [a] -> [a] -> [a]
merge l [] = l
merge [] l = l
merge (x:xs) (y:ys) = if(x<=y) 
        then (x:merge (xs) (y:ys))
            else (y:merge (x:xs) ys)