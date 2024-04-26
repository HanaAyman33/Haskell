--Excercise 1
square x = x*x

cube x = x*x*x

cube2 x = x*(square x)

fourthpower x = square (square x)

isTriple x y z = if square z == square x 
        + square y then True else False

-------------------------------------------------
--Excercise 2

threeDifferent m n p = if 
    n==p && m==p && n==m then False else True
threeDifferent2 m n p  | m==n && n==p && p==n = False
                       | otherwise = True

-------------------------------------------------
--Excercise 3

smaller x y
          | x <= y = x
          | otherwise = y

minThree x y z  | x<=y && x<=z = x
                | y<=x && y<=z = y
                | otherwise = z

minThree2 x y z = smaller x (smaller y z)

-------------------------------------------------
--Excercise 4

sumsq 1 = 1
sumsq x = (x*x) + sumsq (x-1)

fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

isPrime 2 = True
isPrime 3 = True
isPrime x = if hasNoFactor x (div x 2) then True else False

hasNoFactor x 2 = mod x 2/=0
hasNoFactor x n = if mod x n ==0 then False 
                else hasNoFactor x (n-1)

primeGEQ x =  if isPrime (x) then x else primeGEQ (x+1)

--IMPORTANT--
gcd1 a b = if a<b then gcd1 b a 
            else if b==0 then a 
                else gcd1 b (mod a b)

power a 0 = 1
power a 1 = a
power a b = if even b then square (power a (div b 2))
            else a * power a (b-1)

-------------------------------------------------

--Excercise 5

last1 [] = error "the list is empty"
last1 ([h]) = h
last1 (h:t) = last1 t

--occursIn x xs = elem x xs
occursIn x [] = False
occursIn x (h:t) = if x==h then True else occursIn x t 

occurs a [] = []
occurs x (h:t) = if occursIn x h 
        then (h:(occurs x t))
            else occurs x t

reverse1 [] = []
reverse1 (h:t) = reverse1 t ++ [h]
--Another solution
reverse2 [] = []
reverse2 l = last1 l : reverse2 (init l)

--IMPORTANT--
maxList [] = 0
maxList (x:xs)  | maxList xs > x = maxList xs
                |otherwise  = x

updatePrices _ [] = []
updatePrices p ((str,x):t) =  
        (str,x+x*(p/100)):updatePrices p t
 
--------------------------------------------------

--Excercise 6

palindrome :: String -> Bool
palindrome str  = reverse str == str

--------------------------------------------------

--Excercise 7

prefix (x:xs) (y:ys) = if(x==y) 
            then prefix xs ys   
                else False    

--------------------------------------------------

--Excercise 8

insert x [] = [x]
insert x (h:t) = if(x<=h) 
        then (x:h:t)
            else (h:insert x t)

sort1 [] = []
sort1 (x:xs) = insert x (sort1 xs)

--------------------------------------------------

--Excercise 9

merge l [] = l
merge [] l = l
merge (x:xs) (y:ys) = if(x<=y) 
        then x:merge (xs) (y:ys)
            else y:merge (x:xs) ys