double x = x+x

quadruple x = double(double x)

absolute x = if(x>=0) then x else (-x)

signum x    | x>0 = 0
            | x==0 = 0
            | x<0 = (-x)

fac x = if x==0 then 1 else x*fac(x-1)
--same function as fac
fac2 0 = 1
fac2 x = x*fac2(x-1)

sumAll [] = 0   
sumAll (h:t) = h + sumAll t

doubleList [] = []
doubleList (h:t) = h*2 : doubleList t

tail1 [] = error "The list is empty!!"
tail1 (h:t) = t

flatten1 [] = []
flatten1 (h:t) = h ++ flatten1 t 

getEven [] = []
getEven (h:t) = if mod h 2==0 then h:getEven t else getEven t

getEven2 [] = []
getEven2 (h:t)  | even h = h:getEven2 t
                | otherwise = getEven2 t

isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (h:t) = if h==last t then 
        isPalindrome (init t) else False

zip2 [] [] = []
zip2 [] l = []
zip2 l [] = []
zip2 (h1:t1) (h2:t2) = (h1,h2):zip2 t1 t2

unzip2 [] = ([][])
unzip2 ((a,b):t) = (a:t1,b:t2) where (t1,t2) = unzip2 t