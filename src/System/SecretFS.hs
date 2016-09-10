{-# LANGUAGE OverloadedStrings #-}

module System.SecretFS(
  createSecretFS,
  SecretFSConfig(..)
  ) where

import qualified Data.ByteString.Char8 as BS

import Control.Applicative
import Control.Exception(handle,catch,fromException,SomeException)
import Control.Monad
import Data.ByteString.Char8(pack)
import Data.List(nubBy)
import Data.Maybe(fromJust)
import Foreign.C.Error
import GHC.IO.Exception(IOException(..))
import System.IO
import System.Posix.Types(ByteCount,FileOffset)
import System.Posix.Files
import System.Directory
import System.FilePath.Posix

import System.Fuse

type HT = ()

data SecretFSConfig = SecretFSConfig {
  sc_srcDir :: FilePath,
  sc_log :: BS.ByteString -> IO ()
  }

type State = SecretFSConfig

createSecretFS :: SecretFSConfig -> IO (FuseOperations Handle)
createSecretFS config = do
  let state = config
  return $ defaultFuseOps
    { fuseGetFileStat        = secretGetFileStat state
    , fuseOpen               = secretOpen state
    , fuseFlush              = secretFlush state
    , fuseRead               = secretRead state
    , fuseOpenDirectory      = secretOpenDirectory state
    , fuseReadDirectory      = secretReadDirectory state
    , fuseGetFileSystemStats = secretGetFileSystemStats state
    }

secretGetFileStat :: State -> FilePath -> IO (Either Errno FileStat)
secretGetFileStat state path = do
  sc_log state (BS.pack ("secretGetFileStat " ++ path ++ " -> " ++ (realPath state path)))
  exceptionToEither state (convertStatus <$> getFileStatus (realPath state path))

secretOpen :: State -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno Handle)
secretOpen state path mode flags = do
  sc_log state "secretOpen"
  exceptionToEither state (openFile (realPath state path) ReadMode)

secretFlush :: State -> FilePath -> Handle -> IO Errno
secretFlush state _ h =  catch (hClose h >> return eOK) (exceptionHandler state)

secretRead  :: State -> FilePath -> Handle -> ByteCount -> FileOffset -> IO (Either Errno BS.ByteString)
secretRead state path h byteCount offset = do
  sc_log state "secretRead"
  exceptionToEither state $ do
    hSeek h AbsoluteSeek (fromIntegral offset)
    BS.hGet h (fromIntegral byteCount)

secretOpenDirectory :: State -> FilePath -> IO Errno
secretOpenDirectory state path = do
  sc_log state (BS.pack ("secretOpenDirectory " ++ path ++ " -> " ++ (realPath state path)))
  handle (exceptionHandler state) $ do
    stats <- getFileStatus (realPath state path)
    case isDirectory stats of
      True -> return eOK
      False -> return eNOENT

secretReadDirectory :: State ->FilePath -> IO (Either Errno [(FilePath, FileStat)])
secretReadDirectory state path = do
  sc_log state (BS.pack ("secretReadDirectory " ++ path ++ " -> " ++ (realPath state path)))
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
  sc_log state "secretGetFileSystemStats"
  return $ Right FileSystemStats
    { fsStatBlockSize  = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount  = 5      -- FIXME
    , fsStatFilesFree  = 10     -- FIXME
    , fsStatMaxNameLength = 255 -- FIXME
    }

realPath :: State -> FilePath -> FilePath
realPath state (c:path) | c == '/' = sc_srcDir state </> path
realPath state path = sc_srcDir state </> path

exceptionToEither :: State -> IO a -> IO (Either Errno a)
exceptionToEither state ioa = catch (Right <$> ioa) (\e -> Left <$> exceptionHandler state e)

exceptionHandler :: State -> SomeException -> IO Errno
exceptionHandler state e = case fromException e of
   (Just e) -> case ioe_errno e of
     (Just errno) -> return (Errno errno)
     Nothing -> do
       sc_log state (BS.pack ("io exception without errno: " ++ show e))
       return eFAULT
   Nothing -> do
     sc_log state (BS.pack ("exception: " ++ show e))
     return eFAULT

convertStatus :: FileStatus -> FileStat
convertStatus status = 
  let entryType | isRegularFile status = RegularFile
                | isNamedPipe status = NamedPipe
                | isCharacterDevice status = CharacterSpecial
                | isDirectory status  = Directory
                | isBlockDevice status = BlockSpecial
                | isSymbolicLink status = SymbolicLink
                | isSocket status = Socket
                | otherwise = Unknown
  in FileStat
    { statEntryType = entryType
    , statFileMode  = fileMode status
    , statLinkCount = linkCount status
    , statFileOwner = fileOwner status
    , statFileGroup = fileGroup status
    , statSpecialDeviceID = specialDeviceID status
    , statFileSize  = fileSize status
    , statBlocks    = 1 -- FIXME
    , statAccessTime= accessTime status
    , statModificationTime = modificationTime status
    , statStatusChangeTime = statusChangeTime status
    }


