-- the simple version
multiples :: Int -> DFA
multiples n
    | (n `mod` 2 == 0) && (n /= 2) = (new_states, "10", new_transitions, 0, [0])
    | otherwise = (states, "10", transitions, 0, [0])
    where states = [0,1..n-1]
          transitions = trans states (n)
          (last_states, _, last_trans, _, _) = multiples' (n `div` 2)
          new_state = length last_states 
          new_state_trans = [((new_state, '1'), 1), ((new_state, '0'), 0)]
          new_states = (last_states ++ [new_state])
          new_transitions = (change_transitions last_trans new_state) ++ new_state_trans
          
-- change the one goes to zero to the new state
change_transitions :: [((Int, Char), Int)] -> Int -> [((Int, Char), Int)]
change_transitions trans ns = [to_ns tran ns | tran <- trans ]

to_ns :: ((Int, Char), Int) -> Int -> ((Int, Char), Int)
to_ns ((fs, al), ts) ns
    | (ts == 0) && (fs /= 0)  = ((fs, al), ns)
    | otherwise = ((fs, al), ts)
    
-- test the two version of multiples, see if they are the same
test_multiples :: [Int] -> Bool
test_multiples [] = True
test_multiples (n:ns)
    | ((multiples' n) == (multiples n)) = test_multiples ns
    | otherwise = False
