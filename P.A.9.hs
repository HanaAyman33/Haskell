--Excercise 1
square1 :: (Num a) => [a] -> [a]
square1 [] = []
square1 (x:xs) = (x^2):(square1 xs)

square2 :: (Num a) => [a] -> [a]
square2 [] = []
square2 l = map (^2) l

--Excercise 2
filtereven :: (Integral a) => [a] -> [a]
filtereven [] = []
filtereven (x:xs) | even x = x:filtereven xs
                  | otherwise = filtereven xs
as1 :: (Integral a) => [a] -> [a]
as1 [] = []
as1 (x:xs) = if even x then x:as1 xs else as1 xs

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 _ [] = []
filter2 p (x:xs) = if (p x) then x:(filter2 p xs)
                   else filter2 p xs

filtereven2 :: Integral a => [a] -> [a]
filtereven2 = filter even

--Excercise 3
sumP :: (Int,Int) -> Int
sumP (x,y) = x+y
sumPairs :: [(Int, Int)] -> [Int]
sumPairs l = map sumP l
sumPairs2 :: [(Int, Int)] -> [Int]
sumPairs2 [] = []
sumPairs2 ((x,y):xs) = x+y:sumPairs2 xs

--Excercise 4 --Important--
deleteSpaces :: [Char] -> [Char]
deleteSpaces [] = []
deleteSpaces l = filter (/=' ') l

--Excercise 5
sumHelper :: Num a => [a] -> a
sumHelper [] = 0
sumHelper (x:xs) = x + sumHelper xs
deepSum0 :: Num a => [[a]] -> a
deepSum0 [] = 0
deepSum0 (x:xs) = sumHelper x + deepSum0 xs

deepSum1 [] = 0
deepSum1 (x:xs) = foldr (+) 0 (foldr (++) [] (x:xs))
--deepSum2 :: Num a => [[a]] -> a
--deepSum2 x = foldr (+) 0 (map (foldr (+) 0) x)
--deepSum3 :: Num a => [[a]] -> a
--deepSum3 x = sum (map sum x)

--Excercise 7
zipWith1 :: (Int -> Int -> Int) -> [Int] -> [Int] -> [Int]
zipWith1 _ _ [] = []
zipWith1 _ [] _ = []
zipWith1 f (x1:xs1) (x2:xs2) = (f x1 x2):(zipWith1 f xs1 xs2)

--Excercise 8
forsome _ [] = False
--forsome p l = any p l
forsome p (x:xs) = if (p x) then True else forsome p xs

--Excercise 9
forAll _ [] = True
--forAll p l = all p l
forAll p (x:xs) = if(p x) then forAll p xs else False