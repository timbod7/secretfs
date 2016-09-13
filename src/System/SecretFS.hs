{-# LANGUAGE OverloadedStrings #-}

module System.SecretFS(
  createSecretFS,
  SecretFSConfig(..)
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Applicative
import Control.Exception(handle,catch,throwIO)
import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import System.IO
import System.Posix.Types(ByteCount,FileOffset,EpochTime)
import System.Posix.Files
import System.Directory
import System.FilePath
import System.Fuse

import System.SecretFS.Core
import System.SecretFS.DirConfig
import System.SecretFS.RegularFile
import System.SecretFS.EncryptedFile
import System.SecretFS.InterpolatedFile

data SecretFSConfig = SecretFSConfig {
  sc_srcDir :: FilePath,
  sc_keyPhrase :: KeyPhrase,
  sc_log :: BS.ByteString -> IO ()
  }

createSecretFS :: SecretFSConfig -> IO (FuseOperations SHandle)
createSecretFS config = do
  state <- State
    <$> pure (sc_srcDir config)
    <*> pure (sc_keyPhrase config)
    <*> pure (sc_log config)
    <*> atomically (newTVar M.empty)
  return $ defaultFuseOps
    { fuseGetFileStat        = secretGetFileStat state
    , fuseOpen               = secretOpen state
    , fuseFlush              = secretFlush state
    , fuseRead               = secretRead state
    , fuseWrite              = secretWrite state
    , fuseSetFileSize        = secretSetFileSize state
    , fuseOpenDirectory      = secretOpenDirectory state
    , fuseReadDirectory      = secretReadDirectory state
    , fuseGetFileSystemStats = secretGetFileSystemStats state
    }

secretGetFileStat :: State -> FilePath -> IO (Either Errno FileStat)
secretGetFileStat state path = logcall "secretGetFileStat" state path $ do
  exceptionToEither state (convertStatus <$> getFileStatus (realPath state path))

secretOpen :: State -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno SHandle)
secretOpen state path mode flags = exceptionToEither state $ logcall "secretOpen" state path $ do
  ftype <- getFileType state path
  case ftype of
    Regular ->   regularFile state path mode flags
    Encrypted -> encryptedFile state path (s_keyPhrase state)
    Interpolated -> interpolatedFile state path mode flags

secretRead  :: State -> FilePath -> SHandle -> ByteCount -> FileOffset -> IO (Either Errno BS.ByteString)
secretRead state path sh byteCount offset = exceptionToEither state (sh_read sh byteCount offset)

secretWrite  :: State -> FilePath -> SHandle -> BS.ByteString -> FileOffset -> IO (Either Errno ByteCount)
secretWrite state path sh content offset = exceptionToEither state (sh_write sh content offset)

secretFlush :: State -> FilePath -> SHandle -> IO Errno
secretFlush state _ sh =  exceptionToErrno state (sh_flush sh)

secretSetFileSize :: State -> FilePath -> FileOffset -> IO Errno
secretSetFileSize state path offset = exceptionToErrno state $ logcall "secretSetFileSize" state path $ do
  ftype <- getFileType state path
  case ftype of
    Regular ->   regularFileSetSize state path offset
    Encrypted -> throwIO (SException "unimplemented" (Just path) eFAULT)
    Interpolated -> throwIO (SException "unimplemented" (Just path) eFAULT)

secretOpenDirectory :: State -> FilePath -> IO Errno
secretOpenDirectory state path = logcall "secretOpenDirectory" state path $ do
  handle (exceptionHandler state) $ do
    stats <- getFileStatus (realPath state path)
    case isDirectory stats of
      True -> return eOK
      False -> return eNOENT

secretReadDirectory :: State ->FilePath -> IO (Either Errno [(FilePath, FileStat)])
secretReadDirectory state path = logcall "secretReadDirectory" state path $ do
  let basedir = realPath state path
  exceptionToEither state $ do
    contents <- getDirectoryContents basedir
    forM (filter nonSpecial contents) $ \file -> do
      stats <- getFileStatus (basedir </> file)
      return (file,convertStatus stats)
  where
    nonSpecial "." = False
    nonSpecial ".." = False
    nonSpecial _ = True

secretGetFileSystemStats :: State -> String -> IO (Either Errno FileSystemStats)
secretGetFileSystemStats state str = do
  s_log state "secretGetFileSystemStats"
  return $ Right FileSystemStats
    { fsStatBlockSize  = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount  = 5      -- FIXME
    , fsStatFilesFree  = 10     -- FIXME
    , fsStatMaxNameLength = 255 -- FIXME
    }

