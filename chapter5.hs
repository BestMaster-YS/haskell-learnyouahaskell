chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n:chain (n `div` 2)
  | odd  n = n:chain (n*3 + 1)

numLongChain :: Int
numLongChain = length (filter isLong (map (length . chain) [1..100]))
  where
    isLong v = v >= 15


sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

-- foldl 实现elem
elem' :: (Eq a) => a -> [a] -> Bool
elem' x xs = foldl (\acc cur -> if x == cur then True else acc) False xs

-- foldr 和 foldl 实现map不同
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\cur acc -> f cur:acc) []
-- foldl 实现 map' f = foldl (\acc cur -> acc ++ [f cur])

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 (\ac v -> if ac > v then ac else v)

reverse' :: [a] -> [a]
reverse' = foldl (\ac v -> v:ac ) []
-- reverse' = foldl (flip (:)) []


product' :: (Num a) => [a] -> a
product' = foldr1 (*)


filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x:acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)


-- 自然数平方和第几个超1000的

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
