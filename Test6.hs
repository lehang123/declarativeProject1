module Test6 where


fun :: [Bool] -> Bool
fun [w,x,y,z] = c
    where e = nz && not ((yox) && (wony))
          a = e || (z && xoyow)
          f = x || yz
          g = a && nots
          d = xoyow && nots
          c = not ((b && not (wony && (wonz))) || (y && w))  -- not (( w || ny ) && ( w || nz))
          b = f && (x || wonz) && nots
          xoyow =  w || yox
          yz = ny || z
          yox = y || x
          nots = not (y && x && z)         -- not (x && z)
          nz = not z
          ny = not y
          wony = w || ny
          wonz = w || nz

--  c =  (not wox || yz) && (nxnz || y)      not ((w || x) && (y || z) && not (y && z))
-- c = not ((nw && b && yoz) || (y && w))
-- e = not ((y || z || x) && (w || yz))   nz && not ((y || x) && (w || ny))

-- f && not (nwox && yoz) && nots

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