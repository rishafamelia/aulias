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

map' x = x

--pembatas

filter' x = x

--pembatas

delete x = x

--pembatas

deleteAll' x = x

--pembatas

foldl' x = x

--pembatas

foldl1' x = x

--pembatas

zip' x = x

--pembatas

zipWith' x = x

--pembatas

nth' x = x

--pembatas

scanl' x = x

--pembatas

scanl1' x = x

--pembatas

elem' x = x

--pembatas

notElem' x = x

--pembatas

head' (x:xs) = x

--pembatas

length' [] = 0
length' (x:xs) = 1 + length' xs

--pembatas

reverse' x = x

--pembatas

last' x = x

--pembatas

tail x = x

--pembatas

init' x = x

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

intersperse' x = x

--pembatas

intercalate' x = x

--pembatas

and' x = x

--pembatas

or' x = x

--pembatas

zip3' x = x

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

concatMap' x = x

--pembatas

all' x = x

--pembatas

any' x = x

--pembatas

insert' x = x

--pembatas

zipWith3' x = x

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

replicate' x = x
