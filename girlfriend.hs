import Data.Char
import System.IO

--main = do
--  handle <- openFile "girlfriend.txt" ReadMode
  --contents <- hGetContents handle
--  putStr contents
--  hClose handle

--main = do
--  withFile "girlfriend.txt" ReadMode
--    (\ handle ->
--       do contents <- hGetContents handle
--          putStr contents)
--
withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
  handle <- openFile path mode
  result <- f handle
  hClose handle
  return result

--
--main = do
--  contents <- readFile "girlfriend.txt"
--  putStr contents
--

--main = do
--  contents <- readFile "girlfriend.txt"
--  writeFile "girlfriendcaps.txt" (map toUpper contents)
--

main = do
  withFile "girlfriend.txt" ReadMode
    (\handle ->
       do hSetBuffering handle $ BlockBuffering (Just 2048)
       contents <- hGetContents handle
       putStr contents)
