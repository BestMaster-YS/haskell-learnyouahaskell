-- input and output
import Data.Char
import Control.Monad (when, forever, forM)

-- main = do
--   line <- getLine
--   if null line
--     then return ()
--     else do
--         putStrLn $ reverseWords line
--         main

-- reverseWords :: String -> String
-- reverseWords = unwords . map reverse . words

-- main = do
--   return ()
--   return "HAHA"
--   line <- getLine
--   return "BLAH BLAH BLAH"
--   return 4
--   putStrLn line

-- main = do
--   a <- return "abc"
--   b <- return "def"
--   putStrLn $ a ++ " " ++ b

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do
  putChar x
  putStr' xs
  
-- main = do
--  colors <- forM [1,2,3,4] (\a -> do
--    putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
--    getLine)
--  putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
--  mapM_ print colors


main = interact responedPalindromes

shortLineOnly :: String -> String
shortLineOnly input =
  let allLines = lines input
      shortLines = filter (\line -> length line < 10) allLines
      result = unlines  shortLines
  in  result


responedPalindromes contents = unlines (map (\xs -> if palindrome xs then "palindrome" else "not a palindrome") (lines contents))
  where
    palindrome line = line == reverse line 
