## Modules

### Loading Modules

导入模语法 **import <module name>**，（在文件的首行）

```haskell
-- 选择性导入
import Data.List (nub, sort) 
-- 选择性忽略
import Data.List hiding (nub)
-- 重命名
import qualified Data.Map -- Data.Map.filter
import qualified Data.Map as M
```

### Data.List

```haskell
-- 通用函数介绍
intersperse :: a -> [a] -> [a]
intersperse '.' "MONKEY"
-- "M.O.N.K.E.Y"

intercalate :: [a] -> [[a]] -> [a]
intercalate " " ["hey", "there", "guys"]
-- "hey there guys"

transpose :: [[a]] -> [[a]]
transpose [[1,2,3],[4,5,6],[7,8,9]]
-- [[1,4,7],[2,4,8],[3,6,9]]

-- foldl' foldr' 是严格执行，直接进行计算的的（无惰性）

concat :: Foldable t => t [a] -> [a]
concat ["foo","bar","car"]
-- "foobarcar"

concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
concatMap (replicate 4) [1..3]
[1,1,1,1,2,2,2,2,3,3,3,3]

and :: Foldable t => t Bool -> Bool
and $ map (>4) [5,6,7,8] -- True

or :: Foldable t => t Bool -> Bool
or $ map (==4) [2,3,4,5,6,1] -- True

any :: Foldable t => (a -> Bool) -> t a -> Bool
any (==4) [2,3,5,6,1,4] -- True

all :: Foldable t => (a -> Bool) -> t a -> Bool
all (==4) [2,3,5,6,1,4] -- False

iterate :: (a -> a) -> a -> [a]
take 10 $ iterate (*2) 1
-- [1,2,4,8,16,32,64,128,256,512]

splitAt :: Int -> [a] -> ([a], [a])
splitAt 3 "heyman"  
-- ("hey","man")

takeWhile :: (a -> Bool) -> [a] -> [a]

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile (<3) [1,2,2,2,3,4,5,4,3,2,1]  
-- [3,4,5,4,3,2,1]

-- span 类似于 takeWhile, 它返回一个List元组，第一个是 takeWhile 拿走的，第二个是 takeWhile 丢弃的
span :: (a -> Bool) -> [a] -> ([a], [a])

-- break 则是在第一个为 true 的地方分开
break :: (a -> Bool) -> [a] -> ([a], [a])

sort :: Ord a => [a] -> [a]

-- 将相邻且相等的元素组合在一起
group :: Eq a => [a] -> [[a]]
group [1,1,2,3,6,5,6,4,5,5]
-- [[1,1],[2],[3],[6],[5],[6],[4],[5,5]]


inits :: [a] -> [[a]]
inits "w00t"
--- ["","w","w0","w00","w00t"]

tails :: [a] -> [[a]]
tails "w00t"  
-- ["w00t","00t","0t","t",""]

-- 第一个集合是否为第二个集合的子集
isInfixOf :: Eq a => [a] -> [a] -> Bool -- 中间
isPrefixOf :: Eq a => [a] -> [a] -> Bool -- 开头
isSuffixOf :: Eq a => [a] -> [a] -> Bool -- 结尾

elem :: (Foldable t, Eq a) => a -> t a -> Bool
notElem :: (Foldable t, Eq a) => a -> t a -> Bool

-- 找出全部满足和不满足的元素，放入相应的列表中
partition :: (a -> Bool) -> [a] -> ([a], [a])

find :: (a -> Bool) -> [a] -> Maybe a
find (>4) [1,2,3,4,5,6]
-- Just 5

-- head (dropWhile (\(val,y,m,d) -> val < 1000) stock) 可能会报错，因为如果存在超过1000的股票，就会返回空List, head 会报错

elemIndex :: (Eq a) => a -> [a] -> Maybe Int
elemIndices :: Eq a => a -> [a] -> [Int]

findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndices :: (a -> Bool) -> [a] -> [Int]

-- zip3, zip4, zipWith3, zipWith4 就是对三元组和四元组进行 zip 操作


-- 根据 \n 切断字符串
lines :: String -> [String]

-- 用 \n 组合字符数组
unlines :: [String] -> String

-- 字符串转换为单词
words :: String -> [String]
unwords :: [String] -> String

-- nub 除去相同的值
nub :: (Eq a) => [a] -> [a]

-- delete 删除第一个匹配的值
delete :: Eq a => a -> [a] -> [a]

-- 第一个参数减去第二个参数
(\\) :: Eq a => [a] -> [a] -> [a]
-- [1..10] \\ [2,5,9] === delete 2 . delete 5 . delete 9 $ [1..10]

-- 并集
union :: Eq a => [a] -> [a] -> [a]
[1..10] `union` [5,5,11,11]
-- [1,2,3,4,5,6,7,8,9,10,11]

-- 交集
intersect :: Eq a => [a] -> [a] -> [a]

-- 遍历列表将元素插入第一个较大元素的前面
insert :: Ord a => a -> [a] -> [a]

-- 下面这些函数是已经介绍过的一些函数的通用版，接受返回的类型更加通用
genericLength
genericTake
genericDrop
genericSplitAt
genericIndex
genericReplicate

-- 没有 By 是提供默认的比较函数，下面的函数需要多传一个 compare 函数参数
nubBy, deleteBy, unionBy, intersectBy, groupBy,sortBy, insertBy, maximumBy, minimumBy

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `on` g = \x y -> f (g x) (g y)

-- (==) `on` (> 0) 相当于 \x y -> (x > 0) == (y > 0)
```

### Data.Char

```haskell
isControl :: Char -> Bool
isSpace, isLower, isUpper, isAplha（字母）, isAlphaNum（字母和数字）
isPrint -- 检查字符是否可打印（Control 字符不能打印）
isDigit, isOctDigit, isHexDigit, isLetter, isMark (检查Unicode字符), isNumber
isPunctuation,isSymbol,isSeparator,isAscii,isLatin1,isAsciiUpper,isAsciiLower

-- 返回自定义字符类型GeneralCategory （类似 Ordering）
generalCategory :: Char -> GeneralCategory

toUpper, toLower, toTitle, digitToInt, intToDigit
map toTitle "hey guys" -- "HEY GUYS"
map digitToInt "FF85AB" -- [15,15,8,5,10,11]

ord :: Char -> Int
chr :: Int -> Char

ord 'a'   -- 97  
chr 97    -- 'a'  
```

### Data.Map

```haskell
-- Map 模块中 k 属于 Ord typeclass , 在内部 Map 是由 Tree 实现的
Map.fromList :: Ord k => [(k, a)] -> Map.Map k a

Map.empty -- fromList []

Map.insert :: Ord k => k -> a -> Map.Map k a -> Map.Map k a

-- 检查 map 是否为空
Map.null :: Map.Map k a -> Bool
Map.size :: Map.Map k a -> In
Map.singleton :: k -> a -> Map.Map k a -- 传入key和value返回一个map
Map.lookup :: Ord k => k -> Map.Map k a -> Maybe a
Map.member :: Ord k => k -> Map.Map k a -> Bool -- 检查 key
Map.map :: (a -> b) -> Map.Map k a -> Map.Map k b -- 变换 value
Map.filter :: (a -> Bool) -> Map.Map k a -> Map.Map k a -- 根据 val 刷选
Map.toList :: Map.Map k a -> [(k, a)]
Map.keys :: Map.Map k a -> [k]
Map.elems :: Map.Map k a -> [a]
-- fromList 中相同 key 会被丢掉，fromListWith 可以处理相同的key的val 
Map.fromListWith
	:: Ord k => (a -> a -> a) -> [(k, a)] -> Map.Map k a
-- insertWith = insert + fromListWith
Map.insertWith
  :: Ord k => (a -> a -> a) -> k -> a -> Map.Map k a -> Map.Map k a
```

### Data.Set

```haskell
Set.fromList :: Ord a => [a] -> Set.Set a
-- 交集
Set.intersection :: Ord a => Set.Set a -> Set.Set a -> Set.Set a
-- 在第一个集合中而不再第二个集合中
Set.difference :: Ord a => Set.Set a -> Set.Set a -> Set.Set a
-- 并集
Set.difference :: Ord a => Set.Set a -> Set.Set a -> Set.Set a
null,size, member, empty, singleton, insert, delete
Set.isSubsetOf :: Ord a => Set.Set a -> Set.Set a -> Bool
map, filter
-- 去重先 fromList 再 toList 和 List 中的 nub 函数一样，但是 Set 中是属于类型属于 Ord，List 的 nub 是 Eq 
Set.toList :: Set.Set a -> [a]
let setNub xs = Set.toList $ Set.fromList xs  
setNub "HEY WHATS CRACKALACKIN"  
-- " ACEHIKLNRSTWY"  
nub "HEY WHATS CRACKALACKIN"  
-- "HEY WATSCRKLIN" 
```



### Making our own modules

**文件名和模块名一致**

Geometry.hs

```haskell
    module Geometry  
    ( sphereVolume  
    , sphereArea  
    , cubeVolume  
    , cubeArea  
    , cuboidArea  
    , cuboidVolume  
    ) where  
```

可以以文件夹为单位

在 Geometry 文件夹下，新建 Sphere.hs, Cuboid.hs, Cube.hs

在与 Geomtry 文件夹同等级目录下

```haskell
import Geometry.Sphere  
```









