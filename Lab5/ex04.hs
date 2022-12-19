import Control.Exception
import Data.ByteString qualified as BStr
import System.Environment
import System.IO
import System.IO.Error

{-
main :: IO ()
main = do
  (inFileName:outFileName:_) <- getArgs
  inHdlr <- openFile inFileName ReadMode
  outHdlr <- openFile outFileName WriteMode
  inpStr <- hGetContents inHdlr
  hPutStr outHdlr inpStr
  hClose inHdlr
  hClose outHdlr
-}

{-
main :: IO ()
main = do
  (inFileName:outFileName:_) <- getArgs
  inpStr <- readFile inFileName
  writeFile outFileName inpStr
-}

{-
main :: IO ()
main = do
  (inFileName:outFileName:_) <- getArgs
  inpBStr <- BStr.readFile inFileName
  BStr.writeFile outFileName inpBStr
-}

riskyAction :: IO ()
riskyAction = do
  (fileName : _) <- getArgs
  contents <- readFile fileName
  putStrLn contents

exHdlr :: IOError -> IO ()
exHdlr = \ex ->
  if isDoesNotExistError ex
    then putStrLn "The file doesn't exist!"
    else ioError ex

main :: IO ()
main = do
  result <- try riskyAction
  case result of
    Left ex -> exHdlr ex
    Right _ -> putStrLn "Operation completed"