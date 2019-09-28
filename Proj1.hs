module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Card
import Data.List
import Utils
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
    | otherwise = (pickNextGuess leftOverGS, leftOverGS)
    where leftOverGS = shrinkGameState (guess, gameS) (a, b, c, d, e)

-- testing pick random guess
-- pickRandomGuess :: GameState -> [Card]
-- pickRandomGuess (cs:gs) = cs

-- according to current game state, pick the next best guess, by the one get the most feedback
pickNextGuess :: GameState -> [Card]
pickNextGuess (x:xs) = x

t :: (Int, Int, Int, Int, Int)-> Int
t (a, b, c, d, e) = a + b + c + d + e

-- (length (shrinkGameState (cs, gs) (feedback cs acs)))^2

-- perform elimination of impossible game state
-- 1 args : previous guess and gameState
-- 2 args : feedback from previous guess
-- return possible game state
shrinkGameState :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> GameState
shrinkGameState (guess, gameS) (a, b, c, d, e) = leftGameState
    where lowestRank = (holestR (<) (getR guess))
          highestRank = (holestR (>) (getR guess))
          eliRangeGs = eliByRange ((b, lowestRank), (d, highestRank)) (delete (sort guess) gameS)
          eliRankSuitGs = eliByRS ((a, guess), (c, getR guess), (e, getS guess)) eliRangeGs
          leftGameState = eliRankSuitGs

-- eliminate the impossible combinations from gamestate, by the 1st, 3rd and 5th feedback
-- 1st args : tuple (3rd feedback,  current guess rank) (1st feedback,  current guess card)
-- 2nd current game state
-- return new game state after elimination
eliByRS :: ((Int, [Card]), (Int, [Rank]), (Int, [Suit]))-> GameState -> GameState
eliByRS (ctps, rtps, stps) gss =  filter (matchRS (ctps, rtps, stps)) gss -- current version
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

-- determine if the card combination match the suit requirement, to filter out
-- 1st args : tuple (3rd feedback,  current guess suit)
-- 2nd args : the card combination in game state (each)
matchCard :: (Int, [Card]) -> [Card] -> Bool
matchCard (0, ss) cs = not (lcse ss cs) -- since there is 0 match in rank, any combination that contain the ranks in current guess is impossible
matchCard (n, ss) cs = ccombs ss n cs

-- combine matchRank and suit in a function
matchRS :: ((Int, [Card]), (Int, [Rank]), (Int, [Suit])) -> [Card] -> Bool
matchRS (ctps, rtps, stps) cs = (matchRank rtps cs) && (matchSuit stps cs) && (matchCard ctps cs)

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

-- getRanks from cards
getR :: [Card]->[Rank]
getR [] = []
getR ((Card s r):xs) = r:(getR xs)

-- getSuits from cards
getS :: [Card]->[Suit]
getS [] = []
getS ((Card s r):xs) = s:(getS xs)