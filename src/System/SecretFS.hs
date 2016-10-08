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
  sc_log :: LogLevel -> BS.ByteString -> IO ()
  }

createSecretFS :: SecretFSConfig -> IO (FuseOperations SHandle)
createSecretFS config = do
  mountTime <- (fromIntegral . round) <$> getPOSIXTime
  userID <- getRealUserID
  groupID <- getRealGroupID
  dirConfigs <- atomically (newTVar M.empty)
  fileOps <- atomically (newTVar M.empty)
  let state = State
       (sc_srcDir config)
       (sc_keyPhrase config)
       (sc_log config)
       userID
       groupID
       mountTime
       dirConfigs
       fileOps
       (readContent state)
  return (mkFuseOperations config state)

mkFuseOperations :: SecretFSConfig -> State -> FuseOperations SHandle
mkFuseOperations config state = FuseOperations
    { fuseGetFileStat        = getFileStat
    , fuseReadSymbolicLink   = \_ -> Left <$> unimp "fuseReadSymbolicLink"
    , fuseCreateDevice       = createDevice
    , fuseCreateDirectory    = \_ _ -> unimp "fuseCreateDirectory"
    , fuseRemoveLink         = removeLink
    , fuseRemoveDirectory    = \_ -> unimp "fuseRemoveDirectory"
    , fuseCreateSymbolicLink = \_ _ -> unimp "fuseCreateSymbolicLink"
    , fuseRename             = \_ _ -> unimp "fuseRename"
    , fuseCreateLink         = \_ _ -> unimp "fuseCreateLink"
    , fuseSetFileMode        = setFileMode
    , fuseSetOwnerAndGroup   = \_ _ _ -> unimp "fuseSetOwnerAndGroup"
    , fuseSetFileSize        = setFileSize
    , fuseSetFileTimes       = \_ _ _ -> unimp "fuseSetFileTimes"
    , fuseOpen               = open
    , fuseRead               = read
    , fuseWrite              = write
    , fuseGetFileSystemStats = secretGetFileSystemStats state
    , fuseFlush              = flush
    , fuseRelease            = \_ _ -> return ()
    , fuseSynchronizeFile    = \_ _ -> unimp "fuseSynchronizeFile"
    , fuseOpenDirectory      = secretOpenDirectory state
    , fuseReadDirectory      = secretReadDirectory state
    , fuseReleaseDirectory   = \_ -> return eOK
    , fuseSynchronizeDirectory = \_ _ -> unimp " fuseSynchronizeDirectory"
    , fuseAccess             = access
    , fuseInit               = return ()
    , fuseDestroy            = return ()
    }
  where
    getFileStat path = logcall "secretGetFileStat" state path $ do
      exceptionToEither state (getFileOps state path >>= fo_getFileStat)

    createDevice path entryType fileMode deviceId = do
      fo <- getFileOps state path
      exceptionToErrno state (fo_createDevice fo entryType fileMode deviceId)  

    removeLink path = do
      fo <- getFileOps state path
      exceptionToErrno state (fo_removeLink fo)  
      
    setFileMode path fileMode = do
      fo <- getFileOps state path
      exceptionToErrno state (fo_setFileMode fo fileMode)  

    setFileSize  path offset = exceptionToErrno state $ do
      fo <- getFileOps state path
      fo_setFileSize fo offset

    open path mode flags = exceptionToEither state $ do
      fo <- getFileOps state path
      fo_open fo mode flags
  
    flush _ sh =  exceptionToErrno state (sh_flush sh)

    read path sh byteCount offset = exceptionToEither state (sh_read sh byteCount offset)

    write path sh content offset = exceptionToEither state (sh_write sh content offset)

    access path perms = exceptionToErrno state $ do
      fo <- getFileOps state path
      fo_access fo perms

    unimp name = sc_log config LogError ("** BUG: " <> name <> " not implemented") >> return eNOSYS

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
    getFileStat state path = getFileOps state path >>= fo_getFileStat

    nonSpecial "." = False
    nonSpecial ".." = False
    nonSpecial _ = True

secretGetFileSystemStats :: State -> String -> IO (Either Errno FileSystemStats)
secretGetFileSystemStats state str = do
  s_log state LogDebug "secretGetFileSystemStats"
  return $ Right FileSystemStats
    { fsStatBlockSize  = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount  = 5      -- FIXME
    , fsStatFilesFree  = 10     -- FIXME
    , fsStatMaxNameLength = 255 -- FIXME
    }

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
  (Interpolated bindings) -> interpolatedFileOps state path bindings

readContent ::  State -> FilePath -> IO BS.ByteString
readContent state path = do
  fops <- getFileOps state path
  length <- (fromIntegral . statFileSize) <$> fo_getFileStat fops
  sh <- fo_open fops ReadOnly flags
  sh_read sh length 0
  where
    flags = OpenFileFlags False False False False False

