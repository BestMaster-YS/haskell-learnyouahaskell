-- Modules 章节代码
import Data.List
import Data.Function (on)
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub


-- search a list for a sublist
search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
  let nlen = length needle
  in  foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

groupNegAndPos :: (Ord a, Num a) => [a] -> [[a]]
groupNegAndPos = groupBy (\x y -> (x > 0) == (y > 0))

-- 模拟 words
words' = filter (not . any isSpace) . groupBy ((==) `on` isSpace)

encode :: Int -> String -> String
encode shift msg =
  let ords = map ord msg
      shifted = map (+ shift) ords
  in  map chr shifted

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

-- findKey :: (Eq k) => k -> [(k,v)] -> v
-- findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs) = if key == k
                            then Just v
                            else findKey key xs

fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty
