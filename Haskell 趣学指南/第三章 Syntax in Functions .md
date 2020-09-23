## Syntax in Functions

### pattern match

模式匹配（从上至下）

List 的模式匹配，( x:xs  ,  @all(x:xs),  @name指向整个List的引用，++ 不能在List的模式匹配中使用)

### Guards

例子

```haskell
bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!" 
```

### Where

```haskell
    bmiTell :: (RealFloat a) => a -> a -> String  
    bmiTell weight height  
        | bmi <= skinny = "You're underweight, you emo, you!"  
        | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
        | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
        | otherwise     = "You're a whale, congratulations!"  
        where bmi = weight / height ^ 2
              skinny = 18.5  
              normal = 25.0  
              fat = 30.0  
```

**where** 中的变量作用域为整个函数，**where** 绑定是语法结构

### Let



```haskell
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
	let sideArea = 2 * pi * r * h
			topArea = pi * r ^2
	in  sideArea + 2 * topArea
```

表达式格式：   **let** \<**bindings**\>  **in**  \<**expressions**\>     

bindings 中的变量作用域只限于 expressions 中 且 **Let** 绑定是表达式

Let 表达式在 List Comprehension 中可以省略 in 部分（ghci 中同样可以省略），in 的作用可以理解为固定作用域

### Case expressions

Case 表达式格式（模式匹配是 Case 表达式的语法糖）

```haskell
case expression of pattern -> result  
                   pattern -> result  
                   pattern -> result

describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list."

```











