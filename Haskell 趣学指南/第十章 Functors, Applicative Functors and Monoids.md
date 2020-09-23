## Functors, Applicative Functors and Monoids

### Functors redux

将 **Functor** 比喻为盒子只是为了好理解，Functor 的正确理解应该是 computational context(计算语境)，computation 可能带有值，可能失败，可能有多个值

**Functor instance:  IO**

```haskell
instance Functor IO where
	fmap f action = do
		result <- action
		return (f result)

main = do line <- fmap reverse getLine  
   putStrLn $ "You said " ++ line ++ " backwards!"  
   putStrLn $ "Yes, you really said" ++ line ++ " backwards!"  
```

对于 **getLine :: IO String** 可以直接应用 fmap f

**Function instance: (->) r**

对于函数 **r -> a** 可以写成 **(->) r a**，对于 **(->)** 看作为 type constructor，接受两个 type 参数

```haskell
instance Functor ((->) r) where
	fmap f g = (\x -> f (g x))

fmap :: (a -> b) -> f a -> f b
fmap :: (a -> b) -> ((->) r a) -> ((->) r b)
```

事实上，**(->) r** 作为 Functor 的 instance，可以被看作为 composition

```haskell
instance Functor ((->) r) where
	fmap = (.)
```

**fmap :: (a -> b) -> (f a -> f b)** 取函数 a -> b 作为参数，返回函数 f a -> f b

**Functor laws**

1. **fmap id = id**
2. **fmap (f . g) = fmap f . fmap g  或者  fmap (f . g) F = fmap f (fmap g F)**

### Applicative Functors

当我们用需要传入两个参数的函数去 mapping functors 时，例如

```haskell
:t fmap (*) (Just 3) 
-- fmap (*) (Just 3) :: Num a => Maybe (a -> a)
:t fmap compare (Just 'a')  
-- fmap compare (Just 'a') :: Maybe (Char -> Ordering) 
```

Maybe 中含有的是 function

**Applicative typeclass**

```haskell
class (Functor f) => Applicative f where
	pure :: a -> f a
	(<*>) :: f (a -> b) -> f a -> f b
```

Applicative instance Maybe

```haskell
instance Applicative Maybe where
	pure = Just
	Nothing <*> _ = Nothing
	(Just f) <*> a = fmap f a
```

比较发现 **pure f  <*>  x** 与 **fmap f x** 相等 

在 Control.Applicative 模块中

```haskell
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x
```

List instance of Applicative Functor

```haskell
instance Applicative [] where
	pure x = [x]
	fs <*> xs = [f x | f <- fs, x <- xs]
```

IO instance of Applicative Functor

```haskell
instance Applicative IO where
	pure = return
	a <*> b = do
		f <- a
		x <- b
		return (f x)
```

**(->) r**  instance  of  Applicative  Functor

```haskell
instance Applicative ((->) r) where
	pure x = (\_ -> x)
	f <*> g = \x -> f x (g x)
	-- f :: r -> a -> b == (\r -> (\a -> b))
	-- g :: r -> a == \r -> a
	-- h :: r -> b = \r -> f r (g r) == \x -> f x (g x)
```

**liftA2** 函数

```haskell
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b
```

对于  liftA2  函数使我们操作函数和  functor  之间更加方便

当我们想让普通函数操作 functor 时，就需要用到  liftA2 (或者需要用到  <$>)

```haskell
liftA2 (:) (Just 3) (Just [4]) -- Just [3,4]
```

```haskell
sequenceA' :: (Applicative f) => [f a] -> f [a]
-- sequenceA' [] = pure []
-- sequenceA' (x:xs) = (:) <$> x <*> sequenceA xs
sequenceA' = foldr (liftA2 (:)) (pure [])
```

sequenceA' 函数主要是利用 liftA2 (:) 将 List 中的 functor 组合为一个 functor

```haskell
sequenceA' [Just 3, Just 2, Just 1]  
-- Just [3,2,1]
sequenceA' [(+3),(+2),(+1)] 3
-- sequenceA' [(+3),(+2),(+1)] === (:) <$> (+3) <*> (+2)
-- 由于 (:) 是个无限参数函数，所以可以不断的 <*> 操作 functor
```

**Applicative Functor laws**

1. **pure id <*>  v = v**
2. **pure (.) <\*>  u  <\*> v <*> w  = u <\*>  (v  <\*>  w) **
3. **pure  f  <\*>  pure x =  pure (f  x)**
4. **u <\*>  prue y  =  pure ($  y)  <\*> u **

### newtype

对于 Applicative Functor [] 有两种方式去实现  <*> , 第一种为 List 中的每个 f 去与第二个List 中的每个值结合，第二种则是像 zipWith 函数一样，对应一致的函数 f 与值结合。

因为不能对同一个 datatype 实现不同的 <*> 规则，可以新建的一个 List datatype

```haskell
data ZipList a = ZipList [a]
data ZipList a = ZipList { getZipList :: [a] }

-- 使用 newtype
newtype ZipList a = ZipList { getZipList :: [a] }
```

使用 newtype 更加快速，Haskell 知道是想 wrap 一个 type 创造一个 new type。而是用 data wrap ，Haskell 在操作时，还会进行 wrap 和 unwrap 操作

但是对于 newtype ，只能拥有一个 value constructor 并且 value constructor 只能有一个 field。data 可以拥有多个 value constructor 和多个 field。

newtype 可以使用 deriving 派生实例，但是 newtype 包装里的 内部 type 必须是 该 typeclass 的 instance。

```haskell
newtype CharList CharList { getCharList :: [Char] } deriving (Eq, Show)
```

Char 是 Eq, Show 的 instance，才能让 CharList 使用 deriving。

#### Using newtype to make type class instance

对于 tuple (a, b) 若想实现 Functor instance，并且让 f 应用在第一个 component 上，直接对 tuple 实现不太可能。可以使用 newtype 包装，将 type parameter 顺序倒转

```haskell
newtype Pair b a = Pair { getPair :: (a, b) }

instance Functor (Pair c) where
	fmap f (Pair (x, y)) =  Pair (f x, y)


helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

data CoolBool = CoolBool { getCoolBool :: Bool }
-- helloMe undefined :: Exception
newtype CoolBool = CoolBool { getCoolBool :: Bool }
-- helloMe undefined "hello"
```

```haskell
newtype CharList CharList { getCharList :: [Char] } deriving (Eq, Show)
```

CharList 不能使用 [Char]的方法，如 ++

### Monoids

构建一个新的 typeclass，它有如下特性

1. 函数需要两个参数
2. 参数和返回值是相同 type
3. 存在一个参数值，使得输入另外一个参数到函数中，返回的还是另一个值，不会改变
4. 多个参数进行reduce时，可以使用结合律，不影响结果

```haskell
class Monoid m where
	mempty :: m
	mappend :: m -> m -> m
	mconcat :: [m] -> m
	mconcat = foldr mappend mempty
```









