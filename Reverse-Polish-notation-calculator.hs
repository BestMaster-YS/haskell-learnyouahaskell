-- 逆波兰表达式计算
import Data.List

solveRPN :: (Num a, Read a) => String -> a
solveRPN = head . foldl foldingFunction [] . words
  where foldingFunction (x:y:ys) "*" = (x * y):ys
        foldingFunction (x:y:ys) "+" = (x + y):ys
        foldingFunction (x:y:ys) "-" = (x - y):ys
        foldingFunction (x:y:ys) "/" = (x / y):ys
        foldingFunction (x:y:xs) "^" = (x ** y):ys
        foldingFunction (x:xs) "in" = log x:xs
        foldingFunction xs "sum" = [sum xs]
        foldingFunction xs numberString = read numberString:xs

-- TODO 添加错误判断 Maybe
