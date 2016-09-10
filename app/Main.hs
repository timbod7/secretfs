module Main where

import qualified Data.ByteString.Char8 as BS

import System.Environment(getArgs,withArgs)
import System.Exit(exitWith,ExitCode(..))
import System.Fuse(fuseMain,defaultExceptionHandler)
import System.IO
import System.SecretFS

usage :: IO ()
usage = do
  putStrLn "Usage: secretfs SRCDIR MOUNTPOINT"
  exitWith (ExitFailure 1)

logMessage :: Handle -> BS.ByteString -> IO ()
--logMessage h bs = BS.hPutStrLn h bs >> hFlush h
logMessage h bs = BS.putStrLn bs

main :: IO ()
main = do
  args  <- getArgs
  case args of
   (srcdir:mountdir:fuseArgs) -> do
     logh <- openFile "/tmp/secretfs.log" WriteMode
     let config = SecretFSConfig {
           sc_srcDir = srcdir,
           sc_log = logMessage logh
           }
     fuseOperations <- createSecretFS config 
     withArgs (mountdir:fuseArgs) $ fuseMain fuseOperations defaultExceptionHandler
   _ -> usage
