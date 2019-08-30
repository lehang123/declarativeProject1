-- module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Card
import Data.List
-- import HaskellTest
-- import Proj1Test

-- the list of remaining possible answers in the guess
type GameState = [[Card]]

suits = [Club, Diamond, Heart, Spade]
ranks = [R2, R3, R4, R5, R6, R7, R8, R9, R10, Jack, Queen, King, Ace ]

-- initDeck :: [Card]
-- initDeck = [ Card x y | x<-suits, y<-ranks]

initDeck :: [Card]
initDeck = [minBound..maxBound]::[Card]
-- first args : the number of cards in the answer
initGameState :: Int -> GameState
initGameState n = map sort (combinations n initDeck)

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

-- giving feedbacks
-- 1st args : target (answer)
-- 2nd args : guess
feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback [] [] = (0 ,0 ,0 ,0 ,0)
feedback ts gs = ((match ts gs),
                  (lhR (<) (getR ts) (holestR (<) (getR gs))),
                  (match (getR ts) (getR gs)),
                  (lhR (>) (getR ts) (holestR (>) (getR gs))),
                  (match (getS ts) (getS gs)))

initialGuess :: Int -> ([Card],GameState)
initialGuess n = (initialCards n, initGameState n)

-- initalGuessCards
initialCards :: Int -> [Card]
initialCards n  = [Card s r | (s, r) <- (zip (take n suits) (everyNth (13 `div` (n + 1)) ranks)) ]

-- get every nth element frorm list
everyNth :: Int -> [t] -> [t]
everyNth _ [] = []
everyNth n xs = [xs !! i | i <- [n,n+n+1.. (length xs)-1]]

-- performing guesses until it got it right
-- 1 args : previous guess and gameState
-- 2 args : feedback from previous guess
-- return new guess, and new game state
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (guess, gameS) (a, b, c, d, e)
    | a == (length guess) = (guess, [[]]) -- you win, no other possible guess
    | otherwise = (guess, eliRankSuitGs)
    where lowestRank = (holestR (<) (getR guess))
          highestRank = (holestR (>) (getR guess))
          eliRangeGs = eliByRange ((b, lowestRank), (d, highestRank)) (delete (sort guess) gameS)
          eliRankSuitGs = eliByRS ((c, getR guess), (e, getS guess)) eliRangeGs


-- eliminate the impossible combinations from gamestate, by the 3rd and 5th feedback
-- 1st args : tuple (3rd feedback,  current guess rank)
-- 2nd current game state
-- return new game state after elimination
eliByRS :: ((Int, [Rank]), (Int, [Suit]))-> GameState -> GameState
eliByRS (rtps, stps) gss =  filter (matchRS (rtps, stps)) gss -- current version
-- eliByRS (rtps, stps) gss = filter (matchSuit stps) (filter (matchRank rtps) gss) -- separate version

-- determine if the card combination match the rank requirement, to filter out
-- 1st args : tuple (3rd feedback,  current guess rank)
-- 2nd args : the card combination in game state (each)
matchRank :: (Int, [Rank]) -> [Card] -> Bool
matchRank (0, rs) cs = not (lcse rs (getR cs)) -- since there is 0 match in rank, any combination that contain the ranks in current guess is impossible
matchRank (n, rs) cs = ccombs rs n (getR cs)

-- determine if the card combination match the suit requirement, to filter out
-- 1st args : tuple (3rd feedback,  current guess suit)
-- 2nd args : the card combination in game state (each)
matchSuit :: (Int, [Suit]) -> [Card] -> Bool
matchSuit (0, ss) cs = not (lcse ss (getS cs)) -- since there is 0 match in rank, any combination that contain the ranks in current guess is impossible
matchSuit (n, ss) cs = ccombs ss n (getS cs)

-- combine matchRank and suit in a function
matchRS :: ((Int, [Rank]), (Int, [Suit])) -> [Card] -> Bool
matchRS (rtps, stps) cs = (matchRank rtps cs) && (matchSuit stps cs)

-- eliminate the impossible combinations from gamestate, by the 2nd and 4th feedback
-- 1st args : tuple (2nd feedback, lowestRank), (4th feedback, highestRank)
-- 2nd current game state
-- return new game state after elimination
eliByRange :: ((Int, Rank), (Int, Rank)) -> GameState -> GameState
eliByRange tps gss = filter (matchRange tps) gss

-- determine if the card combination match the range requirement, to filter out
-- 1st args : tuple (2nd feedback, lowestRank), (4th feedback, highestRank), total number of cards (2~4)
-- 2nd args : the card combination in game state (each)
matchRange :: ((Int, Rank), (Int, Rank)) -> [Card]  -> Bool
matchRange ((a, x),(b, y)) cs  = ((lhR (<) (getR cs) x) == a) && ((lhR (>) (getR cs) y) == b)

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
ccombs [] _ [] = True
ccombs _ _ [] = False
ccombs [] _ _ = False
ccombs xs n ys =  or [ isInfixOf (sort x) (sort ys) | x <-(combinations n xs)]


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
