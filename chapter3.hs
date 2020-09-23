luckly :: (Integral a) => a -> String
luckly 7 = "luckly Number Seven"
luckly x = "Sorry, you're out of luck, pal!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = x * factorial (x - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

-- let in list comprehension

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]
