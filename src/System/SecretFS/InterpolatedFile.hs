module System.SecretFS.InterpolatedFile(
  interpolatedFileOps
  ) where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Lazy.Encoding as LT


import Control.Exception(throwIO)
import Control.Monad(when)
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Data.Bits((.|.),(.&.),complement)
import System.Fuse(OpenMode,OpenFileFlags)
import System.IO(IOMode(..),SeekMode(..),openFile,hClose,hSeek)
import System.Fuse
import System.Posix.Files(modificationTime,getFileStatus,fileAccess,ownerWriteMode,groupWriteMode,otherWriteMode)
import System.Posix.Types(ByteCount,FileOffset,EpochTime)
import Text.Hastache(hastacheStr,MuConfig(..),emptyEscape)
import Text.Hastache.Aeson(jsonValueContext)

import System.SecretFS.Core

data InterpText = InterpText {
  it_text :: BS.ByteString,
  it_modified :: EpochTime
}

data InterpFileState = InterpFileState {
  ifs_state :: State,
  ifs_template :: FilePath,
  ifs_bindings :: FilePath,
  ifs_interpText ::  Maybe InterpText
  }

type IFStateV = TVar InterpFileState

interpolatedFileOps :: State -> FilePath -> FilePath -> IO FileOps
interpolatedFileOps state path bindings = logcall "interpolatedfile.fileOps" state path $ do
  let ifstate = InterpFileState state path bindings Nothing
  statev <- atomically (newTVar ifstate)
  
  return FileOps {
    fo_open=foOpen statev,
    fo_getFileStat=foGetFileStat statev,
    fo_setFileSize=(\_ -> throwErr statev ePERM "interpolated files are read only"),
    fo_createDevice=(\_ _ _ -> throwErr statev ePERM "can't create interpolated file"),
    fo_access=foFileAccess statev,
    fo_removeLink=throwErr statev ePERM "can't remove interpolated file",
    fo_setFileMode=(\_ -> throwErr statev ePERM "can't chmod interpolated file")
  }

foOpen :: IFStateV -> OpenMode -> OpenFileFlags -> IO SHandle
foOpen statev ReadOnly _ = logcall' "interpolatedfile.openFile" statev $ do
  return SHandle {
  sh_flush = logcall' "interpolatedfile.flush" statev $ return (),
  sh_read = readFile,
  sh_write = (\_ _ -> throwErr statev ePERM "can't write to an interpolated file")
  }
  where
    readFile :: ByteCount -> FileOffset -> IO BS.ByteString
    readFile byteCount offset = logcall' "interpolatedfile.readFile" statev $ do
      itext <- it_text <$> getInterpolatedText statev
      return (BS.take (fromIntegral byteCount) (BS.drop (fromIntegral offset) itext))
foOpen statev _ _ = throwErr statev ePERM "interpolated files are read only"

foGetFileStat :: IFStateV -> IO FileStat
foGetFileStat statev = logcall' "interpolatedfile.getFileState" statev $  do
  rpath <- getRealPath statev
  itext <- getInterpolatedText statev
  stat <- convertStatus <$> getFileStatus rpath
  let fileMode' = statFileMode stat .&.
                  complement (ownerWriteMode .|. groupWriteMode .|. otherWriteMode)
  return stat
    { statFileSize=fromIntegral (BS.length (it_text itext))
    , statFileMode=fileMode'
    , statModificationTime=it_modified itext
    }

foFileAccess :: IFStateV -> Int -> IO ()
foFileAccess statev perms = logcall' "interpolatedfile.fileAccess" statev $ do
  let (readflag,writeflag,execflag) = accessFlags perms
  rpath <- getRealPath statev
  ok <- fileAccess rpath readflag writeflag execflag
  when (writeflag || not ok) (throwErr statev ePERM "Access check failed")

throwErr :: IFStateV -> Errno -> String -> IO a
throwErr statev errno s = do
  path <- atomically (ifs_template <$> readTVar statev)
  throwIO (SException s (Just path) errno)

getRealPath :: IFStateV -> IO FilePath
getRealPath statev = do
  state <- atomically (readTVar statev)
  return (realPath (ifs_state state) (ifs_template state))

getInterpolatedText :: IFStateV -> IO InterpText
getInterpolatedText statev = logcall' "interpolatedfile.getInterpolatedText" statev $ do
  state <- atomically (readTVar statev)
  case ifs_interpText state of
    Nothing -> loadInterpolatedText statev 
    (Just it) -> do
      modifyTime <- getModifyTime statev
      if modifyTime > it_modified it
        then loadInterpolatedText statev
        else (return it)

loadInterpolatedText :: IFStateV -> IO InterpText
loadInterpolatedText statev = logcall' "interpolatedfile.loadInterpolatedText" statev $ do
  state <- atomically (readTVar statev)
  modifyTime <- getModifyTime statev
  let rtemplate = realPath (ifs_state state) (ifs_template state)
      rbindings = realPath (ifs_state state) (ifs_bindings state)
      
  -- read the bindings file into a single json value
  bindings <- do
    bs <- s_readContent (ifs_state state) (ifs_bindings state)
    case JSON.eitherDecode (LBS.fromStrict bs) of
      Left err -> throwIO (SException "can't parse bindings json" (Just rbindings) eFAULT)
      Right jv -> return (jv::JSON.Value)

  -- use hastash to interpolate the template
  template <-  T.decodeUtf8With T.lenientDecode <$> BS.readFile rtemplate
  let config = MuConfig emptyEscape Nothing Nothing (const (return Nothing))
      context = jsonValueContext bindings
  text <- hastacheStr config template context
  let bs = LBS.toStrict (LT.encodeUtf8 text)
  let itext = (InterpText bs modifyTime)

  -- update state
  atomically $ modifyTVar' statev $ \v -> v{
    ifs_interpText=Just itext
    }
  return itext

getModifyTime :: IFStateV -> IO EpochTime
getModifyTime statev = do
  state <- atomically (readTVar statev)
  pathModifyTime <- modificationTime <$> getFileStatus (realPath (ifs_state state) (ifs_template state))
  bindingsModifyTime <- modificationTime <$> getFileStatus (realPath (ifs_state state) (ifs_bindings state))
  return (max pathModifyTime bindingsModifyTime)


logcall' :: String -> IFStateV -> IO a -> IO a
logcall' fnname statev ioa = do
  state <- atomically (readTVar statev)
  logcall fnname (ifs_state state) (ifs_template state) ioa
