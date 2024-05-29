import Text.Parsec.Token (GenLanguageDef(reservedNames))
import System.Posix.Internals (lstat)
import Control.Concurrent (Chan)
import Data.Char (isDigit)
--Write a Haskell function sumAbsDiffs with its type definition that takes as input a list of inte- gers and returns the sum of the absolute differences between each element and its previous element in the list.
sumAbsDiffs :: [Int] -> Int
sumAbsDiffs [] = 0
sumAbsDiffs [_] = 0
sumAbsDiffs (x1:x2:xs) = abs (x1-x2) + sumAbsDiffs (x2:xs)

--Re-implement sumAbsDiffs by utilising the list comprehension notation this time. You can use any predefined functions in this part.
--Hint: Consider using zip.
sumAbsDiffs2 l = sum [abs (y-x) | (x,y) <- zip l (tail l)]

--Implement a function sameSumAbsDiff with its type definition that takes as input a list of lists of integers. The function returns True if all lists in the input list of lists have the same sum of absolute differences between each element and the previous element, and False otherwise. You must use higher-order functions and sumAbsDiffs you previously implemented.
sameSumAbsDiff :: [[Int]] -> Bool
sameSumAbsDiff l = length (filter (/= head l2) l2) == 0
   where l2 = map sumAbsDiffs l

-----------------------------------

--In this exercise, you will implement a menu for a restaurant using Haskell. The menu is represented as a list of dishes. Each dish has a name, a list of ingredients, and a price. Your goal is to define the necessary data types and implement functions to perform various operations on the menu. Provide all the type definitions of the implemented functions.
--a)Define a data type Ingredient that represents a single ingredient. An ingredient should have a name (a string) and a quantity (a floating-point number).
data Ingredient = I String Double deriving Show

--b)Define a data type Dish that represents a single dish on the menu. A dish should have a name (a string), a list of ingredients (a list of Ingredient), and a price (a floating-point number).
data Dish = D String [Ingredient] Double

--c)Define a data type Menu that represents list of dishes.
type Menu = [Dish]

--d) Implement a function parseIngredient that takes a string representation of an ingredient and returns an Ingredient data type. The string representation of an ingredient consists of the ingredient name followed by the quantity. For example, "Tomatoes 2.5" represents 2.5 tomatoes. You can assume that the input string is well-formatted and follows the specified pattern (the ingredient name followed by a single white space followed by the quantity).
--Note: To parse a string s to a double use read s::Double.
parseIngredient :: String -> Ingredient
parseIngredient s = I name quantity 
   where (name,quantity) = parseString s []
parseString (h:t) b = if(h==' ') then (b,read t:: Double)
   else parseString t (b++[h])

--e)Implement a function calculateTotalPrice that takes a menu and calculates the total price of all
--the dishes.
calculateTotalPrice :: Menu -> Double
calculateTotalPrice [] = 0
calculateTotalPrice ((D _ _ d):t) = d + calculateTotalPrice t 

--f) Implement a function findDishesWithIngredient that takes a menu and an ingredient name and returns a list of dishes' names that contain the specified ingredient.
findDishesWithIngredient :: Menu -> String -> [String]
findDishesWithIngredient [] _ = []
findDishesWithIngredient (D name (I n1 _:t1) _:t) n2 = 
   if(n1==n2) then (name :findDishesWithIngredient t n2) 
   else findDishesWithIngredient t n2 

---------------------------------------------------
--Implement a Haskell function noDups which takes as input a list lst and returns another list with the unique elements in lst in the same order they appear in lst.
--Hint: Think of using a pre-defined higher-order function.
noDups [] = []
noDups l = noDupsHelper l l
noDupsHelper :: Eq a => [a] -> [a] -> [a]
noDupsHelper [] l = []
noDupsHelper (h:t) l = if length (filter (==h) l) == 1
      then h:noDupsHelper t l 
      else noDupsHelper t l

-----------------------------------------------

--Write a function rotabc that changes a's to b's, b's to c's and c's to a's in a string. Only lowercase letters are affected.
--1 -> without recusion
rotabc1 :: [Char] -> [Char]
helper h = if(h=='a') then 'b'
      else if(h=='b') then 'c'
      else if(h=='c') then 'a'
      else h 
rotabc1 l = map helper l
--2 -> with recursion
rotabc2 :: [Char] -> [Char]
rotabc2 [] = []
rotabc2(h:t) = if(h=='a') then 'b':rotabc2 t
   else if(h=='b') then 'c':rotabc2 t
   else if(h=='c') then 'a':rotabc2 t
   else h:rotabc2 t

-----------------------------------------------------

data Genre = Nonfiction | Novel | Biography deriving (Eq, Show)
type Name = (String, String)
type Date = (Int, Int, Int) -- day, month, year
data Book = ABook Genre
                  Name -- name of the author
                  String -- title of the book
                  Date -- date of publication
                  Int -- number of pages 
                  deriving Show
--a) Write a function genre that given a book returns its genre.
genre :: Book -> Genre
genre (ABook g _ _ _ _) = g

--b) Write a function title that given a book returns its title. 
title :: Book -> String
title (ABook _ _ t _ _) = t

--c) Write a function date that given a book returns its date. 
date :: Book -> Date
date (ABook _ _ _ d _) = d

--d) Write a function pages that given a book returns its number of pages. 
pages :: Book -> Int
pages (ABook _ _ _ _ p) = p

--e) Write a function year that given a book returns its year of publication. 
year :: Book -> Int
year (ABook _ _ _ (_,_,y) _) = y

--evaluates to a list that contains only those books of the input list that have been published in the year specied by the Int-argument.
helpIn y b = y == year b
publishedIn :: Int -> [Book] -> [Book]
publishedIn y l = filter (helpIn y) l

--counts the number of pages of all books of the input list
totalPages :: [Book] -> Int 
totalPages l = foldr (+) 0 (map pages l)

--computes a list of all titles of the given genre
titlesOf :: Genre -> [Book] -> [String] 
eqG g b = g == genre b
titlesOf g l = map title (filter (eqG g) l)

------------------------------------------------------

--write a function returns a list that looks like xs except that the leftmost item in xs that satisfies predicate p is replaced
--by val. If none of the elements of xs satisfy p, the xs is returned.
changeFirst :: (Int -> Bool) -> Int-> [Int] -> [Int]
changeFirst _ _ [] = []
changeFirst p val (h:t) = if(p h) then (val:t)
   else h:(changeFirst p val t)

------------------------------------------------------

--Write a function f :: [Int] -> [Int] -> Int that computes the sum of the numbers in its first argument that are divisible by the number at the corresponding position in its second argument. If the lengths of the two lists do not match, the extra elements in the longer list are ignored. Assume that none of the numbers in the second argument are 0
divides (a,b) = mod a b==0
f x y =  foldr (+) 0 (map (\(x,y)->x) (filter divides (zip x y )))
--another solution
f2 x y = foldr (+) 0 (map selectX (zip x y))
selectX (x,y) = if mod x y == 0 then x else 0

--Write a second function g :: [Int] -> [Int] -> Int that behaves like f, this time using basic functions and recursion but not higher order library functions. i.e. Do not use any higher order library functions.
g :: [Int] -> [Int] -> Int
g [] _ = 0
g _ [] = 0
g (n:ns) (m:ms) |mod n m ==0 = n + g ns ms
                | otherwise = g ns ms

------------------------------------------------------
type Number = Int
type Point = (Number,Number)
type Length = Number
data Shape = Pt Point
           | Circle Point Length
           | Rect Point Length Length
           deriving Show
type Figure = [Shape] 
type BBox = (Point,Point)
--a) Define the functions width, bbox, and minX that compute the width, bounding box, and minimum x coordinate of a shape, respectively.
width :: Shape -> Length
width (Pt(x,y)) = 0
width (Circle (x,y) r) = 2*r
width (Rect (x,y) w h) = w
bbox :: Shape -> BBox
bbox (Pt(x,y)) = ((x,y),(x,y))
bbox (Circle (x,y) r) = ((x-r,y-r),(x+r,y+r))
bbox (Rect (x,y) w h) = ((x,y),(x+w,y+h))
minX :: Shape -> Number
minx (Pt (x,y)) = x
minX (Circle (x,y) r) = x-r
minX (Rect (x,y) w h) = x
--b) Define a function addPt, which adds two points component wise.
addPt :: Point -> Point -> Point
addPt (x,y) (x1,y1) = (x+x1,y+y1)
--c) Define a function move that moves the position of a shape by a vector given by a point.
move :: Shape -> Point -> Shape
move (Pt(x,y)) (px,py)= Pt(x+px,y+py)
move (Circle (x,y) r) (px,py) = (Circle (x+px,y+py) r)
move (Rect (x,y) w h) (px,py)= (Rect (x+px,y+py) w h)

triangle :: Int -> [[Int]]
triangle n = [ [1..x] | x <- [1..n] ]

-----------------------------------------------

--Suppose the playing cards in a standard deck are represented by characters in the following way: '2' through '9' plus '0' (zero) stand for the number cards, with '0' representing the 10, while the 'A', 'K', 'Q' and 'J' stand for the face cards, i.e. the ace, king, queen and jack, respectively. Let's call these the card characters. The other characters, including the lowercase letters 'a', 'k', 'q', and 'j', and the digit '1', are not used to represent cards.
--a) Write isFaceCard :: Char -> Bool to test whether a character stands for a face card.
isFaceCard :: Char -> Bool
isFaceCard x = x == 'A' || x == 'K' || x == 'Q' || x == 'J'

--b) Write isCard :: Char -> Bool to test whether a character stands for a card. For example,
isCard :: Char -> Bool
isCard x = isFaceCard x || (isDigit x && x /= '1')

--c) Write a function f :: String -> Bool to test whether all card characters in a string represent face
--cards.
f3 :: String -> Bool
f3 [] = True
f3 (c:str) | isCard c  = isFaceCard c && f3 str
          | otherwise = f3 str

--d) Write a function g :: String -> Bool that behaves like f but using one or more of the following
--higher-order functions:map,filter and foldr
h :: String -> Bool
h str = foldr (&&) True (map isFaceCard (filter isCard str))

-----------------------------------------------
--Write a function that,givenanon-emptylistofnumbers,returnsTrueifeachsuccessive number (except the first) is at least twice its predecessor in the list. The function should give an error if applied to the empty list
k :: [Int] -> Bool
k [] = error "Cannot apply function to an empty list"
k [_] = True
k (x:x1:t) = if(x1>=2*x) then k (x1:t) else False

-----------------------------------------------
--Write a function p :: [Int] -> Int that computes the sum of the cubes of the positive numbers in a list.Applicative
--Use higher order function only: map,filter and foldr
cube x =  x*x*x
r xs = foldr (+) 0 (map cube (filter (>0) xs))

-------------------------------------------------

--A boolean expression/proposition is an expression that results in a boolean value; either true or false.
--The following datatype represents boolean expressions/propositions.
data Proposition = Var String    -- Atom
                 | F             -- False
                 | T             -- True
                 | Not Proposition     -- Negation
                 | Proposition :|: Proposition     -- Disjunction (OR)
                 | Proposition :&: Proposition     -- Conjuction (AND)


--a) Write a function  that returns true when a proposition is in nega- tion normal form. A proposition is in negation normal form if the only occurrences of logical negation (Not) are applied to variables
isNorm :: Proposition -> Bool
isNorm (Var x) = True
isNorm T = True
isNorm F = True
isNorm (Not (Var x)) = True
isNorm (Not x) = False
isNorm (p :|: q) = isNorm p && isNorm q
isNorm (p :&: q) = isNorm p && isNorm q

--b)Write a function norm :: Proposition -> Proposition that converts a proposition to an equi- valent proposition in negation normal form.
--Not F <-> T
--Not T <-> F
--Not (Not p) <-> p
--Not (p :|: q) <-> Not p :&: Not q
--Not (p :&: q) <-> Not p :|: Not q

norm :: Proposition -> Proposition
norm (Var x) = Var x
norm T = T
norm F = T
norm (Not (Var x)) = Not (Var x)
norm (Not T) = F
norm (Not F) = T
norm (Not (Not p)) = norm p
norm (Not (p :|: q)) = norm (Not p) :&: norm (Not q)
norm (Not (p :&: q)) = norm (Not p) :|: norm (Not q)
norm (p:|:q) = norm p:|:norm q
norm (p:&:q) = norm p:&:norm q 
