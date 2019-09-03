module Utils (lhR, holestR, match, matchS, combinations, ccombs, lcse, slcse) where

import Data.List
import Card
-- get the number of value in the answer lower/highest than the lowest/highest value in the guess
-- first args : operator used (> for highest, < for lowest)
-- second args : the answer cards
-- third args : the lowest rank
lhR ::(Ord a)=> (a -> a -> Bool) -> [a] -> a -> Int
lhR _ [] _ = 0
lhR (op) (x:xs) (gr)
    | x `op` gr = 1 + (lhR (op) (xs) (gr))
    | otherwise = lhR (op) xs (gr)

-- get the lowest/highest value in the cards
-- first argument : operator used (> for highest, < for lowest)
-- cards to input
holestR ::(Ord a)=> (a -> a -> Bool)->[a]->a
holestR _ [r] = r
holestR op (x:xs)
    | x `op` (holestR (op) xs) = x
    | otherwise = holestR (op) xs

match :: (Ord a) => [a] -> [a] -> Int
match xs ys = matchS (sort xs) (sort ys)

-- find match values, no duplicate. NOTED : list have to be sorted !!!
-- first argument : targetValues.
-- second argument : guessValues.
matchS :: (Ord a) => [a] -> [a] -> Int
matchS [] _ = 0
matchS _ [] = 0
matchS (x:xs) (y:ys)
    | x == y = 1 + matchS (xs) (ys)
    | x > y = matchS (x:xs) (ys)
    | x < y = matchS (xs) (y:ys)

-- a function to make n combinations in a list
-- first args : n
-- second args : the list
combinations :: (Ord a)=> Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs, ys <- combinations (n-1) xs']

-- check if a list contain one of those combination in the other list
-- first args : the list of combinations
-- second args : the n combinations
-- third args : check list
ccombs :: (Ord a)=> [a]-> Int ->[a]->Bool
ccombs xs n ys = (match xs ys) == n


-- check if list contains same element
lcse :: (Ord a) => [a] -> [a] -> Bool
lcse xs ys = slcse (sort xs) (sort ys)

-- check if list contains at least one same element (sorted list only)
slcse :: (Ord a)=> [a] -> [a] -> Bool
slcse [] [] = True
slcse _ [] = False
slcse [] _ = False
slcse (x:xs) (y:ys)
    | x == y = True
    | x > y = slcse (x:xs) (ys)
    | x < y = slcse (xs) (y:ys)