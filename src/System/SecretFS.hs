{-# LANGUAGE OverloadedStrings #-}

module System.SecretFS(
  createSecretFS,
  SecretFSConfig(..)
  ) where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Applicative
import Control.Exception(handle,catch,fromException,throwIO,SomeException,Exception)
import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TVar
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
  = Encrypted
  | Interpolated

data SHandle = SHandle {
  sh_flush :: IO (),
  sh_read :: ByteCount -> FileOffset -> IO BS.ByteString
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
    , fuseOpenDirectory      = secretOpenDirectory state
    , fuseReadDirectory      = secretReadDirectory state
    , fuseGetFileSystemStats = secretGetFileSystemStats state
    }

-- | Get the configuration for a directory, caching
-- the result based upon file modification time
getDirConfig :: State -> FilePath -> IO DirConfig
getDirConfig state path = do
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
      
loadDirConfig :: State -> FilePath -> IO DirConfig
loadDirConfig = undefined

regularFile :: State -> FilePath -> IO SHandle
regularFile state path = do
  h <- (openFile (realPath state path) ReadMode)
  return SHandle {
    sh_flush = hClose h,
    sh_read = \byteCount offset -> do
      hSeek h AbsoluteSeek (fromIntegral offset)
      BS.hGet h (fromIntegral byteCount)
    }

data EncFileState = EncFileState
  { efs_cleartext :: BS.ByteString
  , efs_dirty :: Bool
  }
                    
encryptedFile :: State -> FilePath -> KeyPhrase -> IO SHandle
encryptedFile state path keyphrase = do
  exists <- doesFileExist rpath
  istate <- if exists
    then do
      clearText <- readEncryptedFile rpath keyphrase
      return (EncFileState clearText False)
     else do
      return (EncFileState "" True)
              
  efstate <- atomically $ newTVar istate
  return SHandle {
    sh_flush = flushFile efstate,
    sh_read = readFile efstate
    }

  where
    rpath :: FilePath
    rpath = realPath state path

    readEncryptedFile :: FilePath -> KeyPhrase -> IO BS.ByteString
    readEncryptedFile = undefined

    writeEncryptedFile :: FilePath -> KeyPhrase -> IO BS.ByteString
    writeEncryptedFile = undefined

    readFile :: TVar EncFileState -> ByteCount -> FileOffset -> IO BS.ByteString
    readFile = undefined

    flushFile :: TVar EncFileState -> IO ()
    flushFile = undefined

interpolatedFile :: State -> FilePath -> IO SHandle
interpolatedFile = undefined

secretGetFileStat :: State -> FilePath -> IO (Either Errno FileStat)
secretGetFileStat state path = do
  s_log state (BS.pack ("secretGetFileStat " ++ path ++ " -> " ++ (realPath state path)))
  exceptionToEither state (convertStatus <$> getFileStatus (realPath state path))

secretOpen :: State -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno SHandle)
secretOpen state path mode flags = exceptionToEither state $ do
  dc <- getDirConfig state (takeDirectory (realPath state path))
  case M.lookup (takeFileName path) (dc_fileTypes dc) of
    Nothing -> regularFile state path
    (Just magicType) -> case magicType of
      Encrypted -> encryptedFile state path (s_keyPhrase state)
      Interpolated -> case mode of
        ReadOnly -> interpolatedFile state path
        _ -> throwIO (SException "Interpolated files are readonly" (Just path) ePERM)

secretFlush :: State -> FilePath -> SHandle -> IO Errno
secretFlush state _ sh =  catch (sh_flush sh >> return eOK) (exceptionHandler state)

secretRead  :: State -> FilePath -> SHandle -> ByteCount -> FileOffset -> IO (Either Errno BS.ByteString)
secretRead state path sh byteCount offset = exceptionToEither state (sh_read sh byteCount offset)

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

-- type AccessTimes = M.Map FilePath EpochTime

-- data CachedValue a = CachedValue {
--   cv_value :: TVar a,
--   cv_accessTime :: TVar AccessTimes,
--   cv_calculate :: IO (a,AccessTimes)
--   }

-- cvReadfile :: FilePath -> IO (CachedValue BS.ByteString)
-- cvReadfile path = newCachedValue $ do
--   modTime <- modificationTime <$> getFileStatus path
--   bs <- BS.readFile path
--   return (bs, M.singleton path modTime)

-- newCachedValue :: IO (a,AccessTimes) -> IO (CachedValue a)
-- newCachedValue = undefined

-- getCV :: CachedValue a -> IO a
-- getCV = undefined

-- instance Functor Cached where
--   fmap f dv = dv{dv_value=f (dv_value dv),dv_calculate=fmap (fmap f) (dv_calculate dv)}

-- data DerivedValue a = DerivedValue {
--   dv_value :: a,
--   dv_accessTimes :: (M.Map FilePath EpochTime),
--   dv_calculate :: IO (DerivedValue a)
-- }
  
-- instance Functor DerivedValue where
--   fmap f dv = dv{dv_value=f (dv_value dv),dv_calculate=fmap (fmap f) (dv_calculate dv)}

-- instance Applicative DerivedValue where
--   pure v = DerivedValue v M.empty (return (pure v))
--   af <*> av = DerivedValue {
--     dv_value=dv_value af (dv_value av),
--     dv_accessTimes=M.unionWith min (dv_accessTimes af) (dv_accessTimes av),
--     dv_calculate = do
--       af' <- dv_calculate af
--       av' <- dv_calculate av
--       return (af <*> av)
--     }


-- cachedReadFile :: FilePath -> IO (IO BS.ByteString)
-- cachedReadFile path = do
--   modTime <- modificationTime <$> getFileStatus
--   bs <- BS.readFile path
--   v <- atomically (newTVar (modTime,bs))
--   return $ do
--     (modTime,bs) <- atomically (readTVar v)
--     modTime' <- modificationTime <$> getFileStatus
--     if modTime' <= modTime
--        then return bs
--        else do
--          bs' <- BS.readFile path
--          atomically (writeVar v (modTime',bs'))
--          return bs'
                    
         
    
  


