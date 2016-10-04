{-# LANGUAGE OverloadedStrings #-}
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
logMessage h bs = BS.hPutStrLn h bs >> hFlush h
--logMessage h bs = BS.putStrLn bs

main :: IO ()
main = do
  args  <- getArgs
  case args of
   (srcdir:mountdir:fuseArgs0) -> do
     logh <- openFile "/tmp/secretfs.log" WriteMode
     let config = SecretFSConfig {
           sc_srcDir = srcdir,
           sc_keyPhrase = "xyzzy", -- FIXME
           sc_log = logMessage logh
           }

         -- direct_io is needed to prevent os caching
         -- from delaying changes
         fuseArgs = "-o":"direct_io":fuseArgs0
     fuseOperations <- createSecretFS config 
     withArgs (mountdir:fuseArgs) $ fuseMain fuseOperations defaultExceptionHandler
   _ -> usage
