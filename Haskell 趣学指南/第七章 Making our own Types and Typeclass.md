## Making Our Own Types and Typeclasses

### Abgebraic data type intro

**data** 关键字定义 type，用于创建一个 new data type, 在 **=** 的右边为 **value constructors** ， 定义 type 可以拥有不同的 value

```haskell
-- 定义 Bool type 拥有 False 或 True 
data Bool = False | True

-- 定义 Int type
data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647

-- 定义形状 (circle 或 rectangle)
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
-- Circle 和 Rectangle 都是 value constructor（也是函数）接受参数返回Shape data type 的值
Circle :: Float -> Float -> Float -> Shape
Rectangle :: Float -> Float -> Float -> Float -> Shape
```

注意 Circle 和 Rectangle 不是 type，是 Shape type 中的 value constructor，并且在模式匹配中可以使用 value constructor

```haskell
surface :: Shape -> Float  
surface (Circle _ _ r) = pi * r ^ 2  
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1) 
```

Shape type 可以属于 Show typeclass，就可以实现 show 方法进行打印

将Point type 分离出来，在 Point Type 中 type name 和 value constructor name 一致（需要在只有一个 value constructor 情形下） 

 ```haskell
data Point = Point Float Float deriving (Show)  
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float  
surface (Circle _ r) = pi * r ^ 2  
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape  
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r  
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))   
 ```

导出 data type 和 value constructor

```haskell
module Shapes
( Point(..)
, Shape(Circle, Rectangle)  -- 可以用 .. 将 value constructor 全部导出
, surface
, nudge
, baseCircle
, baseRect
) where
```

### Record Syntax

**Record syntax** 用于便捷的创建拥有复杂属性的 data type，更加清晰

 ```haskell
data Person = Person String String Int Float String String deriving (Show)
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show)
:t firstName -- firstName :: Person -> String

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

Car {company="Ford", model="Mustang", year=1967}  
-- Car {company = "Ford", model = "Mustang", year = 1967}
 ```

Haskell 会根据 record syntax 自动生成对应的函数，实例化方式不同，并且 deriving Show 后，show 的结果也会不同

### Type parameters

value constructor 可以接受参数，并生成新的值，而 type constructor 可以接受 type parameter 生成新 type。

```haskell
data Maybe a = Nothing | Just a
```

其中 a 是 type parameter，Maybe 是 type constructor

Type parameter 适合用于不关心内部value的type

```haskell
data (Ord k) => Map k v = ...
```

**(Ord k) => ** 这部分叫做 typeclass constraint

### Derived instance

read 将字符串转换为类型时，需要声明类型

```haskell
read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person
```

为 Ord typeclass 的实例时，若该 type 有不同的 value constructor 时，默认的第一个 value constructor 较小

```haskell
data Bool = False | True deriving (Ord)
True `compare` False -- GT
True > False -- True
```

### Type synonyms

```haskell
type String = [Char]
-- 参数化
type AssocList k v = [(k,v)]
-- partially apply
type IntMap v = Map Int v
```

Either

```haskell
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
```

### Recursive data structures

```haskell
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
```

定义中缀操作符

```haskell
-- infixr 右结合，infixl 左结合
-- infix [digit](代表优先级) opt(符号) 
infixr 5 :-:  
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

-- ++ 操作符定义
infixr 5  ++ 
(++) :: [a] -> [a] -> [a]  
[]     ++ ys = ys  
(x:xs) ++ ys = x : (xs ++ ys)  
```

定义二叉搜索树

```haskell
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
  
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right)
    
treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a left right)  
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right

let nums = [8,6,4,1,7,3,5]
-- foldr 是因为前面定义 insertTree a tree (value在前，accumulate在后)
let numsTree = foldr treeInsert EmptyTree nums
```

### Typeclass 102

先分析 **Eq** typeclass , 它定义了 **==, /=**  函数

```haskell
class Eq a where
	(==) :: a -> a -> Bool
	(/=) :: a -> a -> Bool
	x == y = not (x /= y)
	x /= y = not (x == y)
```

在 Eq typeclass 定义中，a 是 type variable

创建一个 Eq typeclass 的 type 实例，若是 Eq typeclass 定义中没有定义 == , /= 函数的联系（x == y = not (x /= y)）则需要实现 ==，/= 两个函数的定义

```haskell
data TrafficLight = Red | Yellow | Green
instance Eq TrafficLight where
	Red == Red = True
	Green == Green = True
  Yellow == Yellow = True
  _ == _ = False
```

**class constraint in class declaration**

```haskell
class (Eq a) => Num a where
```

表明一个 type 是 Num typeclass 的实例，首先必须是 Eq typeclass 的实例

由于 Maybe 是一个 type constructor ，所以不能直接成为 typeclass 的实例，必须传入 type 成为具体的类型

```haskell
instance (Eq m) => Eq (Maybe m) where
	Just x == Just y = x == y
	Nothing == Nothing = True
	_ == _ = False
```

因为在比较 Just 时，运用了 x == y 所以 m 必须是 Eq typeclass 的实例

### YesNo typeclass

```haskell
class YesNo a where
	yesno :: a -> Bool

-- 实例化

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _  = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _ = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True
```

### Functor typeclass

```haskell
class Functor f where
	fmap :: (a -> b) -> f a -> f b
```

在定义中 f 是 type constructor

```haskell
map :: (a -> b) -> [a] -> [b]

instance Functor [] where
	fmap = map

instance Functor Maybe where
	fmap f (Just x) = Just (f x)
	fmap f Nothing = Nothing

instance Functor Tree where  
  fmap f EmptyTree = EmptyTree  
  fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)
  
-- Either
data Either a b = Left a | Right b

instance Functor (Either a) where
	fmap f (Right x) = Right (f x)
	fmap f (Left x) = Left x

-- 对于 Either fmap :: (b -> c) -> Either a b -> Either a c，所以 接受 Left x 则不变，Right b -> Right c 

instance Functor (Map.Map k) where
	fmap f (Map.Map k v) = Map.Map k (f v)
```

### Kinds

Type constructor 类似于函数，value 拥有 type （type 是 little labe），type 同样拥有 little label，叫做 kinds （type of  a type）

```haskell
:k Int -- Int :: *
:k Maybe -- Maybe :: * -> *
:k Maybe Int -- Maybe Int :: *
:k Either -- Either :: * -> * -> *  
```

***** 代表一种 concrete type 

```haskell
class Tofu t where
  tofu :: j a -> t a j

-- :k j :: * -> *
-- :k t :: * -> (* -> *) -> *

-- 所以需要创造一个 kind 为 * -> (* -> *) -> * 的 type

data Frank a b = Frank { frankField :: b a } deriving (Show)

-- :t tofu :: x -> Frank a b
instance Tofu Frank where
  tofu x = Frank x

data Barry t k p = Barry { yabba :: p, dabba :: t k }

-- :t fmap :: (a -> b) -> Barry t k a -> Barry t k b

instance Functor (Barry a b) where
  fmap f Barry{yabba = x, dabba = y} = Barry{yabba = f x, dabba = y}
```

