import System.Environment
import System.Directory
import System.IO
import Data.List

-- 调度分配List
dispatch :: [(String, [String] -> IO ())]
dispatch = [("add", add)
           ,("view", view)
           ,("remove", remove)
           ]


-- 根据参数执行对应的命令
main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args


-- add 命令
add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

-- view 命令
view :: [String] -> IO ()
view [filename] = do
  contents <- readFile filename
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
  putStr $ unlines numberedTasks

-- delete 命令
remove :: [String] -> IO ()
remove [filename, numberstring] = do
  handle <- openFile filename ReadMode
  (tempname, temphandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let number = read numberstring
      todotasks = lines contents
      newtodoitems = delete (todotasks !! number) todotasks
  hPutStr temphandle $ unlines newtodoitems
  hClose handle
  hClose temphandle
  removeFile filename
  renameFile tempname filename
