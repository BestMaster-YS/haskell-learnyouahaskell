## Typeclass

### Type Variables

head 函数的类型

```haskell
head :: [a] -> a
```

head 函数类型中的 a 称为 type variable ，拥有 type variable 的函数称为 polymorphic function

### Typeclass 入门

Typeclass 相当于 interface 定义行为。type 属于 typeclass 就必须实现该 typeclass 定义的行为。

```haskell
(==) :: (Eq a) => a -> a -> Bool
```

`=>` 符号之前的属于 class constraint (约束)

常见的 typeclass ：

Eq (其成员type需实现函数 `==`  `/=`),

Ord ( `>`  `<`  `>=`  `<=` )

```haskell
compare :: Ord a => a -> a -> Ordering
```

compare 函数返回 Ordering type (GT  LT  EQ) 

Show

```haskell
show :: Show a => a -> String
```

Read

```haskell
read :: Read a => String -> a
```

Enum (`succ` 函数和 `pred` 函数)

Bounded (minBound, maxBound 函数是 polymorphic constants)

```haskell
minBound :: (Bounded a) => a
maxBound :: (Bounded a) => a
```

Num(包含实数和整数), Integral (menbers type : Int, Integer), Floating (Float, Double)

```haskell
fromIntegral :: (Num b, Integral a) => a -> b
```

