{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

import Data.Monoid
import Options.Applicative
import System.Environment(getArgs,withArgs)
import System.Exit(exitWith,ExitCode(..))
import System.Fuse(fuseMain,defaultExceptionHandler)
import System.IO
import System.SecretFS
import System.SecretFS.Core

usage :: IO ()
usage = do
  putStrLn "Usage: secretfs [--logfile LOGFILE] [--verbose] SRCDIR MOUNTPOINT"
  exitWith (ExitFailure 1)

data Args = Args {
  srcDir :: FilePath,
  mountDir :: FilePath,
  keyPhrase :: BS.ByteString,
  logfile :: Maybe FilePath,
  logverbose :: Bool
  }

args :: Parser Args
args = Args
  <$> strArgument (metavar "SRCDIR" <> help "The template source tree")
  <*> strArgument (metavar "MOUNTDIR" <> help "The expanded result tree")
  <*> pure "xyzzy"
  <*> ((\s -> if null s then Nothing else Just s) <$> strOption (long "logfile" <> value ""))
  <*> switch (long "verbose")

logMessage :: Handle -> Bool -> LogLevel -> BS.ByteString -> IO ()
logMessage h verbose lvl bs
  | show lvl verbose = BS.hPutStrLn h (logLevel lvl <> ": " <>  bs) >> hFlush h
  | otherwise        = return ()
  where
    logLevel LogDebug = "DEBUG"
    logLevel LogInfo = "INFO"
    logLevel LogError = "ERROR"

    show LogDebug False = False
    show _ _ = True

mkLogger :: Maybe FilePath -> Bool -> IO (LogLevel -> BS.ByteString -> IO ())
mkLogger Nothing _ = return (\_ _ -> return ())
mkLogger (Just logfile) verbose = do
  logh <- openFile logfile WriteMode
  return (logMessage logh verbose) 

run :: Args -> IO ()
run args = do
  log <- mkLogger (logfile args) (logverbose args)
  let config = SecretFSConfig {
        sc_srcDir = srcDir args,
        sc_keyPhrase = keyPhrase args,
        sc_log = log
        }
       -- direct_io is needed to prevent os caching
      -- from delaying changes
      fuseArgs = "-o":"direct_io":[]
  fuseOperations <- createSecretFS config
  log LogInfo ("Mounting template fs " <> BS.pack (srcDir args) <> " at " <> BS.pack (mountDir args))
  withArgs (mountDir args:fuseArgs) $ fuseMain fuseOperations defaultExceptionHandler

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> args)
      ( fullDesc
     <> progDesc "Mirror a directory tree expanding templates and exposing secrets"
     <> header "secretfs" )
