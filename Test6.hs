module Test6 where


fun :: [Bool] -> Bool
fun [w,x,y,z] = c
    where e = not ((z || xoy) && (w || yz))
          a = e || (z && (xoyow))
          f = x || yz
          g = a && nots
          d = xoyow && nots
          c = (e || f) && ( not ((x && yoz) && nots))
          b = e && f || (x && ny) || (w && z)
          xoyow =  xoy || w
          yz = ny || z
          ny = not y
          xoy = x || y
          nots = not ( x && y && z)
          yoz = y || z

--     (not x && not y && not z) || (not w && y && not z)
--     c = (e || f) && ( not ((x && yoz) && nots))   c = (not ((y || z)) && b)

-- fun :: [Bool] -> Bool
-- fun [w,x,y,z] = g
--     where ny = not y
--           nz = not z
--           u1 = ny || z
--           q2 = ny || x
--           u2 = nz || y
--           u = (u1 && u2)
--           nq = ((x && ny) || not q2)
--           e = a && nz
--           g = (b || d) && not (u && x || v)
--           a = g || (y && z)
--           f = x || u1
--           b = q2 &&  (nq || nz || w)
--           v = (w && y)
--           c = not (x || v) || u
--           d = w || ( nq || not u1)

    -- 28 gates used

--      g = (b || d) && not (u && x || v)

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