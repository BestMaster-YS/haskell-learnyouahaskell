## Higher order functions

### Curried functions

1. 柯里化函数
2. 中缀函数的柯里化

```haskell
diviedByTen :: (Floating a) => a -> a
diviedByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])
```

特殊例子：`(-4)` 表示为负4，应用`(subtract 4)`代替

### higher-orderism is in order

以函数为参数或为返回值的高阶函数

```haskell
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
	where
		g y x = f x y
-- flip' f y x = f x y
```

### Map and Filters

```haskell
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
	| p x       = x : filter p xs
	| otherwise = filter p xs


chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
	| even n = n:chain (n `div` 2)
	| odd  n = n:chain (n*3 + 1)
```

### Lambda

匿名函数 (\\参数 ->  函数主体 )

### fold

**fold** 函数遍历元素累计返回单值

```haskell
sum' :: (Num a) => [a] -> a  
sum' xs = foldl (\acc x -> acc + x) 0 xs
    
elem' :: (Eq a) => a -> [a] -> Bool  
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map' :: (a -> b) -> [a] -> [b]  
map' f xs = foldr (\x acc -> f x : acc) [] xs

```

**foldl1 和 foldr1**函数主要是不提供初始值，让列表的第一个值作为初始值

**scan** 函数是遍历列表并将中间值(accumulator)记录在列表中,**scanl 将从左往右遍历push记录值，scanr从右往左遍历prepend记录**

```haskell
scanl (+) 0 [3,5,2,1]
-- [0,3,8,10,11]
scanr (+) 0 [3,5,2,1]
-- [11,8,3,1,0]
```

### Function application with $

```haskell
($) :: (a -> b) -> a -> b
f $ x = f x
```

**$** 函数应用拥有最低级的优先级，并且遵循右结合规则，**$** 可以当做函数对待作为参数传递

### Function composition

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

-- 太长的组合链影响可理解性

oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

oddSquareSum =
	let oddSquares = filter odd $ map (^2) [1..]
			belowLimit = takeWhile (<10000) oddSquares
	in  sum belowLimit
```









