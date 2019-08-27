-- module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Card
import Data.List
-- import HaskellTest
-- import Proj1Test

-- the list of remaining possible answers in the guess
type GameState = [[Card]]

-- first args : the number of cards in the answer
-- todo : initGameState :: Int -> GameState
-- initGameState n =

-- The answerer begins by selecting some number of cards from his or her deck without showing the guesser.

-- 5 feedbacks
-- how many cards in the guess are correct (done)

-- how many cards in the answer have rank lower than the lowest rank in the guess.(done)
    -- Ranks, in order from low to high, are 2–10, Jack, Queen, King, and Ace.

-- How many of the cards in the answer have the same rank as a card in the guess (correct ranks).
    --  For this, each card in the guess is only counted once.
    --  That is, if the answer has two queens and the guess has one,
    --  the correct ranks number would be 1, not 2. Likewise if there is one queen in the answer and two in the guess.

-- How many cards in the answer have rank higher than the highest rank in the guess(higher ranks).

-- How many of the cards in the answer have the same suit as a card in the guess,
--  only counting a card in the guess once (correct suits).
    -- For example, if the answer has two clubs and the guess has one club,
    --  or vice versa, the correct suits number would be 1, not 2.

feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback [] [] = (0 ,0 ,0 ,0 ,0)
feedback ts gs = ((match (sort ts) (sort gs)),
                  (lhR (<) (getR ts) (holestR (<) (getR gs))),
                  (match (sort (getR ts)) (sort (getR gs))),
                  (lhR (>) (getR ts) (holestR (>) (getR gs))),
                  (match (sort (getS ts)) (sort (getS gs))))

-- initialGuess :: Int -> ([Card],GameState)
-- initialGuess n =
-- nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)

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

-- getRanks from cards
getR :: [Card]->[Rank]
getR [] = []
getR ((Card s r):xs) = r:(getR xs)

-- getSuits from cards
getS :: [Card]->[Suit]
getS [] = []
getS ((Card s r):xs) = s:(getS xs)

-- remove duplicates from list
rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

-- find match values, no duplicate. NOTED : list have to be sorted !!!
-- first argument : targetValues.
-- second argument : guessValues.
match :: (Ord a) => [a] -> [a] -> Int
match [] _ = 0
match _ [] = 0
match (x:xs) (y:ys)
    | x == y = 1 + match (xs) (ys)
    | x > y = match (x:xs) (ys)
    | x < y = match (xs) (y:ys)



