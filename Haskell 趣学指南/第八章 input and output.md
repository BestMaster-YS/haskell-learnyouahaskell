## Input and Output

### hello world

```haskell
main = putStrLn "hello world"

:t putStrLn -- putStrLn :: String -> IO ()
```

其中 IO action 拥有一个 **type of ()**，() 不仅是一个空 tuple 而且也是一个 type, 因为在屏幕上输出没有什么返回值可言，就为 （），IO action 只有在 main 函数中才能运行，一个 main 函数只能有一个 IO action，而 `do` 可以绑定多个 IO action

```haskell
main = do
	putStrLn "Hello, what's your name?"  
  name <- getLine  
  putStrLn ("Hey " ++ name ++ ", you rock!")

:t getLine -- getLine :: IO String
```

do 作用域块的最后一个 IO action 不能绑定值

```haskell
main = do   
	line <- getLine  
	if null line  
		then return ()  
		else do  
			putStrLn $ reverseWords line  
			main  

reverseWords :: String -> String  
reverseWords = unwords . map reverse . words  
```

注意 return () 为创造一个没有值得 IO action ，就是 IO ()

```haskell
main = do
  return ()
  return "HAHA"
  line <- getLine
  return "BLAH BLAH BLAH"
  return 4
  putStrLn line
```

在程序中执行 return () 并不会结束程序，return 的主要作用是返回包裹一个value的IO action，**return** 和 **<-** 相反

```haskell
-- 有用的IO 函数
putStr :: String -> IO () -- 不换行
putChar :: Char -> IO ()

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do
  putChar x
  putStr' xs

print :: Show a => a -> IO ()
getChar :: IO Char
-- getChar 可能会输入一串字符，按上 return 后，但会转化为字符流，一个一个由 getChar 获取

when :: Applicative f => Bool -> f () -> f ()
-- when 函数 === if something then do some I/O action else return () 
sequence :: [IO a] -> IO [a] -- IO a 会以此执行，并将结果放入列表包裹进 IO action

sequence (map print [1,2,3,4,5])  
1  
2  
3  
4  
5  
[(),(),(),(),()] 
-- sequence 再执行 print Int 时，将 return () 放入结果中
mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
print :: (Show a) => a -> IO ()
mapM print [1,2,3]  
1  
2  
3  
[(),(),()]
-- 最后返回的是 m (t b) IO [(), (), ()]
mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mapM_ print [1,2,3]  
1  
2
3
-- 最后返回的是 m ()，既没有输出
forever :: Applicative f => f a -> f b
-- 永远反复执行绑定的 IO action
forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b) -- 只是与 mapM 的参数相反
```

### File and streams

```haskell
getContents :: IO String
-- 它是惰性函数，不会立即读取字符串流，当遇到 EOF 时会停止

main = do  
  contents <- getContents  
  putStr (shortLinesOnly contents)  
      
shortLinesOnly :: String -> String  
shortLinesOnly input =   
  let allLines = lines input  
      shortLines = filter (\line -> length line < 10) allLines  
      result = unlines shortLines  
  in  result

interact :: (String -> String) -> IO ()
-- interact 函数主要作用 input -> transform -> output
main = interact shortLinesOnly
```

stdin, stdout

```haskell
import System.IO  

main = do  
	handle <- openFile "girlfriend.txt" ReadMode  
	contents <- hGetContents handle  
	putStr contents  
	hClose handle

openFile :: FilePath -> IOMode -> IO Handle
type FilePath = String
data IOMode = ReadMode | WriteMode | AppendMode | ReadWriterMode
hGetContents :: Handle -> IO String
withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a  
withFile' path mode f = do  
    handle <- openFile path mode   
    result <- f handle  
    hClose handle  
    return result 
```

**hGetLine, hPutStr, hPutStrLn, hGetChar** 分别对应没有h的函数

```haskell
readFile :: FilePath -> IO String
-- 读取文件
writeFile :: FilePath -> String -> IO ()
-- 覆盖写入文件
appendFile :: FilePath -> String -> IO ()
-- 添加写入文件
```

I/O lazy 像是一个管道连接到文件的输出口，它会像管道一样不断的输出文件数据，对于文本文件管道的量为 line-buffering，对于二进制文件管道大小为系统指定的block-buffering

**hSetBuffering** 是来控制 buffering 

```haskell
hSetBuffering :: Handle -> BufferMode -> IO ()

data BufferMode
  = NoBuffering | LineBuffering | BlockBuffering (Maybe Int)
-- NoBuffering 是指定一个字符大小

main = do   
  withFile "something.txt" ReadMode (\handle -> do  
    hSetBuffering handle $ BlockBuffering (Just 2048)  
    contents <- hGetContents handle  
    putStr contents)
```

**hFlush** 函数表示强制输出已经得到的数据（一般情况下是遇到换行符才会输出数据）

```haskell
import System.IO  
import System.Directory  
import Data.List  
  
main = do        
    handle <- openFile "todo.txt" ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let todoTasks = lines contents     
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks     
    putStrLn "These are your TO-DO items:"  
    putStr $ unlines numberedTasks  
    putStrLn "Which one do you want to delete?"     
    numberString <- getLine     
    let number = read numberString     
        newTodoItems = delete (todoTasks !! number) todoTasks     
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile "todo.txt"  
    renameFile tempName "todo.txt" 
```

**openTempFile :: FilePath -> String -> IO (FilePath, Handle)**

**openTempFile** 接受一个目录，文件名称，返回处理该临时文件的IO action（ path handle ）

**getCurrentDirectory :: IO FilePath** 是获取当前的文件夹的函数

**renameFile :: FilePath -> FilePath -> IO ()** , **removeFile :: FilePath -> IO ()** 属于 System.Directory

### Command line arguments

在 **System.Environment** 模块中 **getArgs :: IO [String]** 用来获取命令行参数，**getProgName :: IO String** 用来获取程序名称

```haskell
import System.Environment   
import System.Directory  
import System.IO  
import Data.List  
      
dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("add", add)  
            , ("view", view)  
            , ("remove", remove)  
            ]  
       
main = do  
  (command:args) <- getArgs  
  let (Just action) = lookup command dispatch  
  action args  
      
add :: [String] -> IO ()  
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")  
      
view :: [String] -> IO ()  
view [fileName] = do  
  contents <- readFile fileName  
  let todoTasks = lines contents  
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
      putStr $ unlines numberedTasks  
      
remove :: [String] -> IO ()  
remove [fileName, numberString] = do  
  handle <- openFile fileName ReadMode  
  (tempName, tempHandle) <- openTempFile "." "temp"  
  contents <- hGetContents handle  
  let number = read numberString  
    todoTasks = lines contents  
    newTodoItems = delete (todoTasks !! number) todoTasks  
  hPutStr tempHandle $ unlines newTodoItems  
  hClose handle  
  hClose tempHandle  
  removeFile fileName  
  renameFile tempName fileName 
```

### Randomness

**System.Random 模块中** 

**random :: (Random a, RandomGen g) => g -> (a, g)**

**StdGen** 是 RandomGen 的一个实例 type，

**mkStdGen :: Int -> StdGen** 能返回一个 RandomGen value

**randoms :: (Random a, RandomGen g) => g -> [a]** 可以返回无限的随机数列表

**randomR :: (Random a, RandomGen g) => (a, a) -> g -> (a, g)** randomR 函数生成指定范围内的随机数

**randomRs :: (Random a, RandomGen g) => (a, a) -> g -> [a]** randomRs 则是生成指定范围内无限的随机数

**getStdGen :: IO StdGen** 获取关于 StdGen 的 IO action

**newStdGen :: IO StdGen** 会更新系统全局的 StdGen，即 getStdGen 也会不同

### Bytestrings

String 的每个 char 大小可能不同，Bytestrings 的每个元素只有1 byte 大小，并且 Bytestrings 有 strict 和 lazy

```haskell
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S
```

**B.pack :: [GHC.Word.Word8] -> B.ByteString**

```haskell
B.pack [99,97,110]
-- "can"
```

**B.unpack :: B.ByteString -> [GHC.Word.Word8]**

**B.fromChunks :: [S.ByteString] -> B.ByteString** 

**B.toChunks :: B.ByteString -> [S.ByteString]** 

**cons** 就是相当于List中的  **:** 

**cons :: GHC.Word.Word8 -> S(B).ByteString -> S(B).ByteString**

**B.readFile :: FilePath -> IO B.ByteString** 

```haskell
import System.Environment
import qualified Data.ByteString.Lazy as B

main = do
  (filename1: filename2: _) <- getArgs
  copyFile filename1 filename2

copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do
  contents <- B.readFile source
  B.writeFile dest contents
```

### Exceptions

**catch :: IO a -> (IOError -> IO a) -> IO a** 

```haskell
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
```

