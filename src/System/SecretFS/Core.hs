module System.SecretFS.Core where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M


import Control.Concurrent.STM.TVar
import Control.Exception(handle,catch,fromException,throwIO,SomeException,Exception)
import Data.Bits( (.&.) )
import Data.Typeable
import GHC.IO.Exception(IOException(..),ioException)
import System.Posix.Types(ByteCount,FileOffset,UserID,GroupID,EpochTime,FileMode,DeviceID)
import System.FilePath.Posix( (</>) )
import System.Fuse
import System.Posix.Files
import System.IO(IOMode(..))

type KeyPhrase = BS.ByteString       

data DirConfig = DirConfig {
  dc_fileTypes :: M.Map FilePath MagicFileType
  }

data MagicFileType
  = Regular
  | Encrypted
  | Interpolated FilePath
  deriving (Eq,Show)

data LogLevel = LogDebug | LogInfo | LogError

data State = State {
  s_srcDir :: FilePath,
  s_keyPhrase :: KeyPhrase,
  s_log :: LogLevel -> BS.ByteString -> IO (),
  s_userID :: UserID,
  s_groupID :: GroupID,
  s_mountTime :: EpochTime,
  s_dirConfigs :: TVar (M.Map FilePath (DirConfig,EpochTime)),
  s_fileOps :: TVar (M.Map FilePath (MagicFileType,FileOps)),
  s_readContent :: FilePath -> IO BS.ByteString
  }

data FileOps = FileOps {
  fo_open :: OpenMode -> OpenFileFlags -> IO SHandle,
  fo_getFileStat :: IO FileStat,
  fo_setFileSize :: FileOffset -> IO (),
  fo_createDevice :: EntryType -> FileMode -> DeviceID -> IO (),
  fo_removeLink :: IO (),
  fo_setFileMode :: FileMode -> IO (),
  fo_access :: Int -> IO ()
}

data SHandle = SHandle {
  sh_flush :: IO (),
  sh_read :: ByteCount -> FileOffset -> IO BS.ByteString,
  sh_write :: BS.ByteString -> FileOffset -> IO ByteCount
  }


-- | An IO related exception type that doesn't reference
-- ghc internals
data SException = SException
  { se_message :: String
  , se_path :: Maybe FilePath
  , se_errno :: Errno
  } deriving (Typeable)

instance Show SException where
  show (SException message mpath (Errno e)) = "(SException" ++ show message ++ " " ++ show mpath ++ " " ++ show e ++ ")"

instance Exception SException             

----------------------------------------------------------------------

realPath :: State -> FilePath -> FilePath
realPath state (c:path) | c == '/' = s_srcDir state </> path
realPath state path = s_srcDir state </> path

exceptionToEither :: State -> IO a -> IO (Either Errno a)
exceptionToEither state ioa = catch (Right <$> ioa) (\e -> Left <$> exceptionHandler state e)

exceptionToErrno :: State  -> IO () -> IO Errno
exceptionToErrno state io = catch (io >> return eOK) (exceptionHandler state)

exceptionHandler :: State -> SomeException -> IO Errno
exceptionHandler state e = case fromException e of
   (Just e) -> case ioe_errno e of
     (Just errno) -> return (Errno errno)
     Nothing -> do
       s_log state LogError (BS.pack ("io exception without errno: " ++ show e))
       return eFAULT
   Nothing -> case fromException e of
     (Just (SException message mpath errno)) -> return errno
     Nothing -> do
       s_log state LogError (BS.pack ("exception: " ++ show e))
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

logcall :: String -> State -> FilePath -> IO a -> IO a
logcall fnname state path ioa = do
  s_log state LogDebug (BS.pack (fnname ++ ": " ++ path ++ " -> " ++ realPath state path))
  a <- catch ioa handler
  s_log state LogDebug (BS.pack (fnname ++ ": Done"))
  return a
  where
    handler :: SomeException -> IO a
    handler se = do
       s_log state LogError (BS.pack (fnname ++ ": Failed with " ++ show se))
       throwIO se

accessFlags :: Int -> (Bool,Bool,Bool)
accessFlags perms = (readflag,writeflag,execflag)
  where
    readflag = perms .&. 4 /= 0
    writeflag = perms .&. 2 /= 0
    execflag = perms .&. 1 /=0
