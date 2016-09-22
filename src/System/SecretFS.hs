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
import Data.Monoid
import Data.Time.Clock.POSIX(getPOSIXTime)
import System.IO
import System.Posix.Types(ByteCount,FileOffset,EpochTime,UserID,GroupID,FileMode,DeviceID)
import System.Posix.Files
import System.Posix.User(getRealUserID,getRealGroupID)
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
  mountTime <- (fromIntegral . round) <$> getPOSIXTime
  userID <- getRealUserID
  groupID <- getRealGroupID
  state <- State
    <$> pure (sc_srcDir config)
    <*> pure (sc_keyPhrase config)
    <*> pure (sc_log config)
    <*> pure userID
    <*> pure groupID
    <*> pure mountTime
    <*> atomically (newTVar M.empty)
    <*> atomically (newTVar M.empty)
  return $ FuseOperations
    { fuseGetFileStat        = secretGetFileStat state
    , fuseReadSymbolicLink   = \_ -> Left <$> unimp "fuseReadSymbolicLink"
    , fuseCreateDevice       = secretCreateDevice state
    , fuseCreateDirectory    = \_ _ -> unimp "fuseCreateDirectory"
    , fuseRemoveLink         = \_ -> unimp "fuseRemoveLink"
    , fuseRemoveDirectory    = \_ -> unimp "fuseRemoveDirectory"
    , fuseCreateSymbolicLink = \_ _ -> unimp "fuseCreateSymbolicLink"
    , fuseRename             = \_ _ -> unimp "fuseRename"
    , fuseCreateLink         = \_ _ -> unimp "fuseCreateLink"
    , fuseSetFileMode        = \_ _ -> unimp "fuseSetFileMode"
    , fuseSetOwnerAndGroup   = \_ _ _ -> unimp "fuseSetOwnerAndGroup"
    , fuseSetFileSize        = secretSetFileSize state
    , fuseSetFileTimes       = \_ _ _ -> unimp "fuseSetFileTimes"
    , fuseOpen               = secretOpen state
    , fuseRead               = secretRead state
    , fuseWrite              = secretWrite state
    , fuseGetFileSystemStats = secretGetFileSystemStats state
    , fuseFlush              = secretFlush state
    , fuseRelease            = \_ _ -> return ()
    , fuseSynchronizeFile    = \_ _ -> unimp "fuseSynchronizeFile"
    , fuseOpenDirectory      = secretOpenDirectory state
    , fuseReadDirectory      = secretReadDirectory state
    , fuseReleaseDirectory   = \_ -> unimp "fuseReleaseDirectory"
    , fuseSynchronizeDirectory = \_ _ -> unimp " fuseSynchronizeDirectory"
    , fuseAccess             = \_ _ -> unimp "fuseAccess"
    , fuseInit               = return ()
    , fuseDestroy            = return ()
    }
  where
    unimp name = sc_log config (name <> " not implemented") >> return eNOSYS

secretGetFileStat :: State -> FilePath -> IO (Either Errno FileStat)
secretGetFileStat state path = logcall "secretGetFileStat" state path $ do
  exceptionToEither state (getFileStat state path)

secretCreateDevice :: State -> FilePath -> EntryType -> FileMode -> DeviceID -> IO Errno
secretCreateDevice state path entryType fileMode deviceId = do
  fo <- getFileOps state path
  exceptionToErrno state (fo_createDevice fo entryType fileMode deviceId)  

secretOpen :: State -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno SHandle)
secretOpen state path mode flags = exceptionToEither state $ logcall "secretOpen" state path $ do
  fo <- getFileOps state path
  fo_open fo mode flags
  
secretRead  :: State -> FilePath -> SHandle -> ByteCount -> FileOffset -> IO (Either Errno BS.ByteString)
secretRead state path sh byteCount offset = exceptionToEither state (sh_read sh byteCount offset)

secretWrite  :: State -> FilePath -> SHandle -> BS.ByteString -> FileOffset -> IO (Either Errno ByteCount)
secretWrite state path sh content offset = exceptionToEither state (sh_write sh content offset)

secretFlush :: State -> FilePath -> SHandle -> IO Errno
secretFlush state _ sh =  exceptionToErrno state (sh_flush sh)

secretSetFileSize :: State -> FilePath -> FileOffset -> IO Errno
secretSetFileSize state path offset = exceptionToErrno state $ logcall "secretSetFileSize" state path $ do
  fo <- getFileOps state path
  fo_setFileSize fo offset

secretOpenDirectory :: State -> FilePath -> IO Errno
secretOpenDirectory state path = logcall "secretOpenDirectory" state path $ do
  handle (exceptionHandler state) $ do
    stats <- getFileStatus (realPath state path)
    case isDirectory stats of
      True -> return eOK
      False -> return eNOENT

secretReadDirectory :: State ->FilePath -> IO (Either Errno [(FilePath, FileStat)])
secretReadDirectory state path = logcall "secretReadDirectory" state path $ exceptionToEither state $ do
  let basedir = realPath state path

  -- Read the contents of the directory
  items <- do
    contents <- getDirectoryContents basedir
    forM (filter nonSpecial contents) $ \file -> do
      stat <- getFileStat state (path </> file)
      return (file,stat)

  -- Generate any extra items corresponding to empty encrypted
  -- files
  extraItems <- do
    dc <- getDirConfig state path
    let files = S.fromList (map fst items)
        extraEncFiles = [file | (file,Encrypted) <- M.toList (dc_fileTypes dc), not (S.member file files)]
    forM extraEncFiles $ \file -> do
      stat <- getFileStat state (path </> file)
      return (file,stat)
      
  return (items ++ extraItems)
      
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

getFileStat :: State -> FilePath -> IO FileStat
getFileStat state path = getFileOps state path >>= fo_getFileStat

getFileOps :: State -> FilePath -> IO FileOps
getFileOps state path = do
  ftype <- getFileType state path
  opsmap <- atomically (readTVar (s_fileOps state))
  -- Use the cached operations if there is an entry in the
  -- map and the file type hasnt changed
  case M.lookup path opsmap of
    (Just (ftype',fops)) | ftype' == ftype -> do
      return fops                      
    _ -> do 
      fops <- getFileOps' state path ftype
      atomically $ modifyTVar (s_fileOps state) (M.insert path (ftype,fops))
      return fops

getFileOps' :: State -> FilePath -> MagicFileType -> IO FileOps
getFileOps' state path ftype = case ftype of
  Regular -> regularFileOps state path
  Encrypted -> encryptedFileOps state path
  Interpolated -> interpolatedFileOps state path
