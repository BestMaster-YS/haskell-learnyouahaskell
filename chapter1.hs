doubleMe x = x + x

doubleUs x y = x*2 + y*2

-- List Comprehension [ f x | x <- xs, conditions ]

boomBangs xs = [if x < 10 then "BOOM" else "BANGS" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

-- Tuple 元组

testTuple :: (Int, Int) -> Int
testTuple (x, y) = x + y
