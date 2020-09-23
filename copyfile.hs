import System.Environment
import qualified Data.ByteString.Lazy as B

main = do
  (filename1: filename2: _) <- getArgs
  copyFile filename1 filename2

copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do
  contents <- B.readFile source
  B.writeFile dest contents