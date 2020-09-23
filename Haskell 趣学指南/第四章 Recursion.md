## Recursion

递归实现常用函数

```haskell
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where
    maxTail = maximum' xs
-- maximum' (x:xs) = max x (maximum' xs)


replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x:replicate' (n-1) x
-- 注意 Num 不是 Ord 的子类

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' _ [] = []
take' n (x:xs)
  | n <= 0 = []
  | otherwise = x:take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
  | a == x    = True
  | otherwise = a `elem'` xs

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = smaller ++ [x] ++ bigger
  where
    smaller = quickSort [ s | s <- xs, s <= x ]
    bigger = quickSort [ b | b <- xs, b > x ]
```

