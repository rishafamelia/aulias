module Zeni where
import Data.List

-- 1.a

null' x
    | x == [] = True
    | otherwise = False

--pembatas

take' 0 (x:xs) = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

--pembatas

drop' _ [] = []
drop' 0 (x:xs) = (x:xs)
drop' n (x:xs) = drop' (n-1) xs

--pembatas

fst (x,y) = x

--pembatas

snd' (x,y) = y

--pembatas

map' f [] = []
map' f (x:xs) = f x : map' f xs


--pembatas

filter' f [] = []
filter' f (x:xs)
  | f x == True = [x] ++ filter' f xs
  | f x == False = [] ++ filter' f xs

--pembatas
delete' _ [] = []
delete' n (x:xs)
  | x == n = [] ++ xs
  | x /= n = [x] ++ delete' n xs

--pembatas

deleteAll' x = x

--pembatas

foldl'' f n [] = n
foldl'' f n (x:xs) = f x (foldl'' f n xs)
--pembatas

foldl1'' f [x] = x
foldl1'' f (x:xs) = f x (foldl1'' f xs)

--pembatas

zip' (x:xs) [] = []
zip' [] (a:bs) = []
zip' (x:xs) (a:bs) = [(x,a)] : zip' xs bs

--pembatas

zipWith' f (x:xs) [] = []
zipWith' f [] (a:bs) = []
zipWith' f (x:xs) (a:bs) = [f x xs] ++ zipWith f a bs

--pembatas

nth' (x:xs) n
  | n == 0 = x
  | otherwise nth' (n-1) xs

--pembatas

scanl' x = x

--pembatas

scanl1' x = x

--pembatas

elem' n [] = False
elem' n (x:xs)
  | n == x = True
  | otherwise elem' n xs

--pembatas

notElem' n [] = True
notElem' n (x:xs)
  | n == x = False
  | otherwise notElem' n xs

--pembatas

head' (x:xs) = x

--pembatas

length' [] = 0
length' (x:xs) = 1 + length' xs

--pembatas

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

--pembatas

last' [x] = x
last' (x:xs) = last' xs

--pembatas

tail' (x:xs) = xs

--pembatas

init' [x] = []
init' (x:xs) = x : init' xs

--pembatas

max' a b
  | a > b = a
  | otherwise = b
--pembatas

min' a b
  | a < b = a
  | otherwise = b

--pembatas

concat' x = x

--pembatas
intersperse' _ [] = []
intersperse' n [x] = [x]
intersperse' n (x:xs) = [x] ++ [n] ++ intersperse' n xs

--pembatas

intercalate' x = x

--pembatas

and' [] = True
and' (x:xs)
  | x == False = False
  | otherwise and' xs
--pembatas

or' [] = False
or' (x:xs)
  | x == True = True
  | otherwise or' xs

--pembatas

zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []
zip3' (x:xs) (a:bs) (c:ds) = [(x,a,c)] : zip3' xs bs ds

--pembatas

sum' [] = 0
sum' (x:xs) = x + sum' xs

--pembatas

product' [] = 1
product' (x:xs) = x * product xs

--pembatas

words' x = x

--pembatas

lines' x = x

--pembatas

unlines' x = x

--pembatas

unwords' x = x

--pembatas

takeWhile' x = x

--pembatas

dropWhile' x = x

--pembatas

concatMap' f [] = []
concatMap' f (x:xs) = f x ++ concatMap' f xs
--pembatas

all' x = x

--pembatas

any' x = x

--pembatas
insert' n [] = [n]
insert' n (x:xs)
  | n <= x = [n] ++ (x:xs)
  | n > x = [x] ++ insert' n xs
--pembatas

zipWith3' f [] _ _ = []
zipWith3' f _ [] _ = []
zipWith3' f _ _ [] = []
zipWith3' f (x:xs) (a:bs) (c:ds) = [f x a c] ++ zipWith3' f xs bs ds

--pembatas

-- 1.b

nub' x = x

--pembatas

sort' x = x

--pembatas

minimum' x = x

--pembatas

maximum' x = x

--pembatas

inits' x = x

--pembatas

tails' x = x

--pembatas

union' x = x

--pembatas

intersect' x = x

--pembatas

group' [] = []

--pembatas

splitAt' x = x

--pembatas

partition' x = x

--pembatas
replicate' n (x:xs)
  | n > 0 = [(x:xs)] ++ replicate' (n-1) xs
  | otherwise = []
