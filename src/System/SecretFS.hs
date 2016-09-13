{-# LANGUAGE OverloadedStrings #-}

module System.SecretFS(
  createSecretFS,
  SecretFSConfig(..)
  ) where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Applicative
import Control.Exception(handle,catch,fromException,throwIO,SomeException,Exception)
import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Crypto.RNCryptor.V3.Decrypt(decrypt)
import Crypto.RNCryptor.V3.Encrypt(encrypt)
import Crypto.RNCryptor.Types(newRNCryptorContext,newRNCryptorHeader)
import Data.ByteString.Char8(pack)
import Data.List(nubBy)
import Data.Maybe(fromJust)
import Data.Typeable
import Foreign.C.Error
import GHC.IO.Exception(IOException(..),ioException)
import System.IO
import System.Posix.Types(ByteCount,FileOffset,EpochTime)
import System.Posix.Files
import System.Directory
import System.FilePath.Posix

import System.Fuse

type KeyPhrase = BS.ByteString       

data SecretFSConfig = SecretFSConfig {
  sc_srcDir :: FilePath,
  sc_keyPhrase :: KeyPhrase,
  sc_log :: BS.ByteString -> IO ()
  }

data State = State {
  s_srcDir :: FilePath,
  s_keyPhrase :: KeyPhrase,
  s_log :: BS.ByteString -> IO (),
  s_dirConfigs :: TVar (M.Map FilePath (DirConfig,EpochTime))
  }

data DirConfig = DirConfig {
  dc_fileTypes :: M.Map FilePath MagicFileType
  }

data MagicFileType
  = Regular
  | Encrypted
  | Interpolated

data SHandle = SHandle {
  sh_flush :: IO (),
  sh_read :: ByteCount -> FileOffset -> IO BS.ByteString,
  sh_write :: BS.ByteString -> FileOffset -> IO ByteCount
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

-- | Get the configuration for a directory, caching
-- the result based upon file modification time
getDirConfig :: State -> FilePath -> IO DirConfig
getDirConfig state path = logcall "getDirConfig" state path $ do
  currentModifyTime <- modificationTime <$> getFileStatus (realPath state path)
  let updateConfig = do
       dirConfig <- loadDirConfig state path
       atomically $ modifyTVar' (s_dirConfigs state) $ M.insert path (dirConfig,currentModifyTime)
       return dirConfig
  dirConfigs <- atomically $ readTVar (s_dirConfigs state)
        
  case M.lookup path dirConfigs of
    Nothing -> updateConfig
    (Just (oldDirConfig,oldModifyTime)) -> do
      if currentModifyTime > oldModifyTime
        then updateConfig 
        else return oldDirConfig

-- | Read and parse the configuration for a
-- directory      
loadDirConfig :: State -> FilePath -> IO DirConfig
loadDirConfig state path = logcall "loadDirConfig" state path $ do
  let cpath = realPath state path </> ".secretfs"
  exists <- doesFileExist cpath
  if exists
    then do
      content <- LBS.readFile cpath
      case JSON.decode content of
       (Just dirConfig) -> return dirConfig
       Nothing -> throwIO (SException "Invalide .secretfs file" (Just cpath) eFAULT)
    else do
      return (DirConfig M.empty)

-- Quick and dirty json parsing of the directory
-- config
instance JSON.FromJSON DirConfig where
  parseJSON o = DirConfig <$> JSON.parseJSON o

instance JSON.FromJSON MagicFileType where
  parseJSON (JSON.String s) | s == "encrypted" = pure Encrypted
                            | s == "instance" = pure Interpolated
                            | s == "regular" = pure Regular
                            | otherwise = empty
  parseJSON _ = empty
  
regularFile :: State -> FilePath -> OpenMode -> OpenFileFlags -> IO SHandle
regularFile state path mode flags = logcall "regularFile" state path $ do
  let rpath = realPath state path
  h <- (openFile rpath (convertMode mode flags))
  return SHandle {
    sh_flush = flush h,
    sh_read = read h,
    sh_write = write h
    }
  where
    flush h = logcall "regularFile.flush" state path (hClose h)

    read h byteCount offset = logcall "regularFile.read" state path $ do
      hSeek h AbsoluteSeek (fromIntegral offset)
      BS.hGet h (fromIntegral byteCount)

    write h content offset = logcall "regularFile.read" state path $ do
      hSeek h AbsoluteSeek (fromIntegral offset)
      BS.hPut h content
      return (fromIntegral (BS.length content))

data EncFileState = EncFileState
  { efs_cleartext :: BS.ByteString
  , efs_dirty :: Bool
  }
                    
encryptedFile :: State -> FilePath -> KeyPhrase -> IO SHandle
encryptedFile state path keyphrase = do
  exists <- doesFileExist rpath
  istate <- if exists
    then do
      clearText <- do
        cipherText <- BS.readFile rpath
        return (decrypt cipherText keyphrase)
      return (EncFileState clearText False)
     else do
      return (EncFileState "" True)
              
  efstate <- atomically $ newTVar istate
  return SHandle {
    sh_flush = flushFile efstate,
    sh_read = readFile efstate,
    sh_write = writeFile efstate
    }

  where
    rpath :: FilePath
    rpath = realPath state path

    readFile :: TVar EncFileState -> ByteCount -> FileOffset -> IO BS.ByteString
    readFile efstate byteCount offset = do
      (EncFileState cleartext dirty) <- atomically (readTVar efstate)
      return (BS.take (fromIntegral byteCount) (BS.drop (fromIntegral offset) cleartext))

    writeFile :: TVar EncFileState -> BS.ByteString -> FileOffset -> IO ByteCount
    writeFile efstate content offset = do
      (EncFileState cleartext dirty) <- atomically (readTVar efstate)
      let lencleartext = BS.length cleartext
          lencontent = BS.length content
          offset' = fromIntegral offset

          -- FIXME: efficiency: perhaps a lazy bytestring or some other structure?
          cleartext' = BS.concat
            [ BS.take offset' cleartext
            , if offset' > lencleartext then BS.replicate (offset' - lencleartext) (toEnum 0) else ""
            , content
            , BS.drop (offset' + lencontent) cleartext
            ]
      atomically (writeTVar efstate (EncFileState cleartext' True))
      return (fromIntegral lencontent)

    flushFile :: TVar EncFileState -> IO ()
    flushFile efstate = do
      (EncFileState cleartext dirty) <- atomically (readTVar efstate)
      when dirty $ do
        hdr <- newRNCryptorHeader keyphrase
        let ctx = newRNCryptorContext keyphrase hdr
        let cipherText = encrypt ctx cleartext
        BS.writeFile rpath cipherText

interpolatedFile :: State -> FilePath -> OpenMode -> OpenFileFlags -> IO SHandle
interpolatedFile state path ReadOnly _ = undefined
interpolatedFile _ path _ _ = throwIO (SException "Interpolated files are readonly" (Just path) ePERM)

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

getFileType :: State -> FilePath ->IO MagicFileType
getFileType state path = do
  dc <- getDirConfig state (takeDirectory path)
  case M.lookup (takeFileName path) (dc_fileTypes dc) of
    Nothing -> return Regular
    (Just magicType) -> return magicType

secretFlush :: State -> FilePath -> SHandle -> IO Errno
secretFlush state _ sh =  catch (sh_flush sh >> return eOK) (exceptionHandler state)

secretRead  :: State -> FilePath -> SHandle -> ByteCount -> FileOffset -> IO (Either Errno BS.ByteString)
secretRead state path sh byteCount offset = exceptionToEither state (sh_read sh byteCount offset)

secretWrite  :: State -> FilePath -> SHandle -> BS.ByteString -> FileOffset -> IO (Either Errno ByteCount)
secretWrite state path sh content offset = exceptionToEither state (sh_write sh content offset)

secretSetFileSize :: State -> FilePath -> FileOffset -> IO Errno
secretSetFileSize = undefined

logcall :: String -> State -> FilePath -> IO a -> IO a
logcall fnname state path ioa = do
  s_log state (BS.pack (fnname ++ ": " ++ path ++ " -> " ++ realPath state path))
  a <- catch ioa handler
  s_log state (BS.pack (fnname ++ ": Done"))
  return a
  where
    handler :: SomeException -> IO a
    handler se = do
       s_log state (BS.pack (fnname ++ ": Failed with " ++ show se))
       throwIO se

secretOpenDirectory :: State -> FilePath -> IO Errno
secretOpenDirectory state path = do
  s_log state (BS.pack ("secretOpenDirectory " ++ path ++ " -> " ++ (realPath state path)))
  handle (exceptionHandler state) $ do
    stats <- getFileStatus (realPath state path)
    case isDirectory stats of
      True -> return eOK
      False -> return eNOENT

secretReadDirectory :: State ->FilePath -> IO (Either Errno [(FilePath, FileStat)])
secretReadDirectory state path = do
  s_log state (BS.pack ("secretReadDirectory " ++ path ++ " -> " ++ (realPath state path)))
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

realPath :: State -> FilePath -> FilePath
realPath state (c:path) | c == '/' = s_srcDir state </> path
realPath state path = s_srcDir state </> path

-- An IO related exception type that doesn't reference
-- ghc internals

data SException = SException
  { se_message :: String
  , se_path :: Maybe FilePath
  , se_errno :: Errno
  } deriving (Typeable)

instance Show SException where
  show (SException message mpath (Errno e)) = "(SException" ++ show message ++ " " ++ show mpath ++ " " ++ show e ++ ")"

instance Exception SException             

exceptionToEither :: State -> IO a -> IO (Either Errno a)
exceptionToEither state ioa = catch (Right <$> ioa) (\e -> Left <$> exceptionHandler state e)

exceptionHandler :: State -> SomeException -> IO Errno
exceptionHandler state e = case fromException e of
   (Just e) -> case ioe_errno e of
     (Just errno) -> return (Errno errno)
     Nothing -> do
       s_log state (BS.pack ("io exception without errno: " ++ show e))
       return eFAULT
   Nothing -> case fromException e of
     (Just (SException message mpath errno)) -> return errno
     Nothing -> do
       s_log state (BS.pack ("exception: " ++ show e))
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

convertMode :: OpenMode -> OpenFileFlags -> IOMode
convertMode ReadOnly _ =   ReadMode 
convertMode WriteOnly OpenFileFlags{append=True} = AppendMode
convertMode WriteOnly _ = WriteMode
convertMode ReadWrite _ = ReadWriteMode
