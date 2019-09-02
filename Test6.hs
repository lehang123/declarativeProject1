module Test6 where

fun :: [Bool] -> Bool
fun [w,x,y,z] = d
    where ny = not y
          nz = not z
          nq = not q
          u1 = ny || z
          q2 = ny || x
          u2 = nz || y
          q1 = not x || y
          u = (u1 && u2)
          q = (q1 && q2)
          e = a && nz
          g = (b || d) && not (u && x || v)
          a = g || (y && z)
          f = x || u1
          b = q2 &&  (nq || nz || w)
          v = (w && y)
          c = not (v || x) || u
          d = w || ( nq || not u1)

--           c = not (v||(x && not u))
--          b = q2 && not (q && z && not w)
--          d = w || not ( q && u1)

    -- 29 gates used

im :: Bool -> Bool -> Bool
im a b = not a || b

bim :: Bool -> Bool -> Bool
bim a b = (a `im` b) && (b `im` a)

xor :: Bool -> Bool -> Bool
xor a b = not (bim a b)

list = [[False, False, False, False], -- 0
         [False, False, False, True], -- 1
         [False, False, True, False], -- 2
         [False, False, True, True],  -- 3
         [False, True, False, False], -- 4
         [False, True, False, True],  -- 5
         [False, True, True, False],  -- 6
         [False, True, True, True],   -- 7
         [True, False, False, False],   -- 8
         [True, False, False, True],   -- 9
         [True, False, True, False]   -- dash
         ]
a = [True, False, True, True, False, True, True, True, True, True, False]
b = [True, False, False, False, True, True, True, False, True, True, False]
c = [True, True, True, True, True, False, False, True, True, True, False]
d = [False, False, True, True, True, True, True, False, True, True, True]
e = [True, False, True, False, False, False, True, False, True, False, False]
f = [True, True, False, True, True, True, True, True, True, True, False]
g = [True, False, True, True, False, True, True, False, True, True, False]

run :: [[Bool]] -> [Bool]
run bss = map fun bss