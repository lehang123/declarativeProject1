import Data.List

type State  = Int
type Symbol = Char
type Transn = ((State, Symbol), State)
type DFA    = ([State], [Symbol], [Transn], State, [State])
type Input  = [Symbol]

-- put your solution to challenge 6 here:
multiples :: Int -> DFA
multiples n = (min_states, "10", min_transitions, 0, [0])
    where states = [0,1..n-1]
          transitions = trans states (n)
          min_prestates = loop_equivalent [[0], states\\[0]] transitions
          min_states = [0,1.. ((length min_prestates)-1)]
          min_transitions = min_trans (zip min_states min_prestates) transitions
          
trans :: [Int] -> Int -> [((Int, Char), Int)]
trans [] _ = []
trans (n:ns) s = ((n, '1'), (2*n + 1) `mod` s):((n, '0'), (2*n) `mod` s):(trans ns s)

-- after getting the minimal state, spit out transition
-- 1st arg : minimal state, 2nd arg : current transition
-- return : minimal transition
min_trans :: [(Int, [Int])] -> [((Int, Char), Int)] -> [((Int, Char), Int)]
min_trans xss trans = foldr (++) [] [make_trans (index, xs) xss trans | (index, xs) <- xss]


make_trans :: (Int, [Int]) -> [(Int, [Int])] -> [((Int, Char), Int)] -> [((Int, Char), Int)]
make_trans (n, ns) nss trans = [one_trans, zero_trans]
    where from = ns !! 0
          one_to = (next_state (from, '1') trans)
          zero_to = (next_state (from, '0') trans) 
          one_trans = [((n, '1'), index)| (index, xs) <- nss, (one_to `elem` xs)] !! 0
          zero_trans = [((n, '0'), index)| (index, xs) <- nss, (zero_to `elem` xs)] !! 0

-- keep looping through n_equivalent, until input equal to output,
-- 1 args : 0 equivalent states, separate final state and others
-- 2 args : the transition
-- return : the minimal state
-- then this is our final state
loop_equivalent :: [[Int]] -> [((Int, Char), Int)] -> [[Int]]
loop_equivalent input trans
    | input == output = output
    | otherwise = loop_equivalent output trans
    
    where output = n_equivalent input trans

n_equivalent :: [[Int]] -> [((Int, Char), Int)] -> [[Int]]
n_equivalent xss trans = foldr (++) [] [(split_equivalent xs xss trans)| xs <-xss]

split_equivalent :: [Int] -> [[Int]] -> [((Int, Char), Int)] -> [[Int]]
split_equivalent [] _ _ = []
split_equivalent (x:xs) xss trans = first: (split_equivalent second xss trans)
    where first = (filter (in_equivalent x trans xss) (x:xs))
          second = ((x:xs)\\first)
    
in_equivalent :: Int -> [((Int, Char), Int)] -> [[Int]] -> Int -> Bool
in_equivalent n trans es m = b1 && b2
    where b1 = equivalent es (next_state (n, '1') trans) (next_state (m, '1') trans)
          b2 = equivalent es (next_state (n, '0') trans) (next_state (m, '0') trans)

next_state :: (Int, Char) -> [((Int, Char), Int)] -> Int
next_state k kvs = [v | (key,v)<-kvs, k==key] !! 0

-- check if two states are in the same equivalent
equivalent :: [[Int]] -> Int -> Int -> Bool
equivalent [] _ _ = False
equivalent (xs:xss) n m
    | (n `elem` xs) && (m `elem` xs) = True
    | otherwise = equivalent xss n m
