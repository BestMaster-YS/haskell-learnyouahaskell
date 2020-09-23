import System.Environment
import System.IO
import System.Directory
import System.IO.Error

-- main = do (filename:_) <- getArgs
--           contents <- readFile filename
--           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

-- 可能文件并不存在，会出错

-- 第一种方案是采用 Myabe 或 Either

-- doesFileExist :: FilePath -> IO Bool

-- main = do (filename:_) <- getArgs
--           fileExists <- doesFileExist filename
--           if fileExists
--             then do contents <- readFile filename
--                     putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
--             else putStrLn "The file does't exist!"

-- 第二种方案是使用 exception

main = toTry `catch` handler

toTry :: IO ()
toTry = do
  (filename:_) <- getArgs
  contents <- readFile filename
  putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

-- handler :: IOError -> IO ()
-- handler e = putStrLn "Whoops, had some trouble!"

-- 我们也可以对 handler 接受到 error 进行判断处理

-- isDoesNotExistError :: IOError -> Bool 判断是否为文件不存在的 error
-- ioError :: IOException -> IO a 相当于丢弃了 error

handler :: IOError -> IO ()
handler e
  | isDoesNotExistError e = putStrLn "The file dose't exist"
  | otherwise = ioError e