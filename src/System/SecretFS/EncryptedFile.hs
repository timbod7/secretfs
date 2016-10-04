{-# LANGUAGE OverloadedStrings #-}
module System.SecretFS.EncryptedFile(
  encryptedFileOps
  ) where

import qualified Data.ByteString.Char8 as BS

import Control.Exception(throwIO)
import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Crypto.RNCryptor.V3.Decrypt(decrypt)
import Crypto.RNCryptor.V3.Encrypt(encrypt)
import Crypto.RNCryptor.Types(newRNCryptorContext,newRNCryptorHeader)
import System.Directory(doesFileExist)
import System.Fuse(OpenMode,OpenFileFlags,FileStat(..),EntryType(..),ePERM)
import System.IO(IOMode(..),SeekMode(..),openFile,hClose,hSeek)
import System.Posix.Files(getFileStatus,fileAccess)
import System.Posix.Types(ByteCount,FileOffset,EpochTime)

import System.SecretFS.Core

data EncFileState = EncFileState
  { efs_cleartext :: BS.ByteString
  , efs_dirty :: Bool
  }

encryptedFileOps :: State -> FilePath -> IO FileOps
encryptedFileOps state path = do
  let rpath = realPath state path
  exists <- doesFileExist rpath
  efstate <- if exists
    then do
      cleartext <- decryptReadFile rpath (s_keyPhrase state)
      return (EncFileState cleartext False)
     else do
      return (EncFileState "" True)
  efstatev <- atomically (newTVar efstate)

  return FileOps{
    fo_open=encryptedFileOpen efstatev state path,
    fo_getFileStat=encryptedGetFileStat efstatev state path,
    fo_setFileSize=encryptedFileSetSize efstatev state path,
    fo_createDevice=(\_ _ _ -> throwIO (SException "can't create encrypted file" (Just path) ePERM)),
    fo_access=encryptedFileAccess efstatev state path,
    fo_removeLink=throwIO (SException "can't remove encrypted file" (Just path) ePERM),
    fo_setFileMode=(\_ -> throwIO (SException "can't chmod encrypted file" (Just path) ePERM))
    }

encryptedFileOpen :: TVar EncFileState -> State -> FilePath -> OpenMode -> OpenFileFlags -> IO SHandle
encryptedFileOpen efstatev state path _ _ = do
  return SHandle {
    sh_flush = flushFile,
    sh_read = readFile,
    sh_write = writeFile
    }

  where
    readFile :: ByteCount -> FileOffset -> IO BS.ByteString
    readFile byteCount offset = logcall "encryptedFile.read" state path $ do
      (EncFileState cleartext dirty) <- atomically (readTVar efstatev)
      return (BS.take (fromIntegral byteCount) (BS.drop (fromIntegral offset) cleartext))

    writeFile :: BS.ByteString -> FileOffset -> IO ByteCount
    writeFile content offset = logcall "encryptedFile.write" state path $ do
      let lencontent = BS.length content
      modifyClearText efstatev $ \cleartext -> 
        let lencleartext = BS.length cleartext
            offset' = fromIntegral offset
        -- FIXME: efficiency: perhaps a lazy bytestring or some other structure?
        in BS.concat
             [ BS.take offset' cleartext
             , if offset' > lencleartext then BS.replicate (offset' - lencleartext) (toEnum 0) else ""
             , content
             , BS.drop (offset' + lencontent) cleartext
             ]
      return (fromIntegral lencontent)

    flushFile :: IO ()
    flushFile = logcall "encryptedFile.flush" state path $ do
      writeIfDirty efstatev state path

encryptedGetFileStat :: TVar EncFileState -> State -> FilePath -> IO FileStat
encryptedGetFileStat efstatev state path = do
  efstate <- atomically (readTVar efstatev)
  exists <- doesFileExist rpath
  fileStat <- if exists
    then do
      convertStatus <$> getFileStatus rpath
    else return newFileStat
  let clearTextLength =  BS.length (efs_cleartext efstate)
  return fileStat{statFileSize=fromIntegral clearTextLength}
  where
    rpath = realPath state path
    newFileStat = FileStat
      { statEntryType = RegularFile
      , statFileMode = 0o700
      , statLinkCount = 1
      , statFileOwner = (s_userID state)
      , statFileGroup = (s_groupID state)
      , statSpecialDeviceID = 0
      , statFileSize = 0
      , statBlocks = 0
      , statAccessTime = (s_mountTime state)
      , statModificationTime = (s_mountTime state)
      , statStatusChangeTime = (s_mountTime state)
      }

encryptedFileSetSize :: TVar EncFileState -> State -> FilePath -> FileOffset -> IO ()
encryptedFileSetSize efstatev state path size =  do
  modifyClearText efstatev (BS.take (fromIntegral size))
  writeIfDirty efstatev state path

modifyClearText :: TVar EncFileState ->  (BS.ByteString -> BS.ByteString) -> IO ()
modifyClearText efstatev updatef = do
  atomically $ do
    efstate <- readTVar efstatev
    let cleartext' = updatef (efs_cleartext efstate)
    when (cleartext' /= efs_cleartext efstate) $ do
      writeTVar efstatev efstate{efs_cleartext=cleartext',efs_dirty=True}

writeIfDirty :: TVar EncFileState -> State -> FilePath -> IO ()
writeIfDirty efstatev state path = do
  efstate <- atomically (readTVar efstatev)
  when (efs_dirty efstate) $ do
    encryptWriteFile rpath (s_keyPhrase state) (efs_cleartext efstate)
    atomically $ modifyTVar efstatev (\efs -> efs{efs_dirty=False})
  where
    rpath = realPath state path

decryptReadFile :: FilePath -> KeyPhrase -> IO BS.ByteString
decryptReadFile path keyphrase = do
  cipherText <- BS.readFile path
  return (decrypt cipherText keyphrase)

encryptWriteFile :: FilePath -> KeyPhrase -> BS.ByteString -> IO ()
encryptWriteFile path keyphrase cleartext = do
  hdr <- newRNCryptorHeader keyphrase
  let ctx = newRNCryptorContext keyphrase hdr
  let cipherText = encrypt ctx cleartext
  BS.writeFile path cipherText
  
encryptedFileAccess :: TVar EncFileState -> State -> FilePath -> Int -> IO ()
encryptedFileAccess _ state path perms = logcall "encryptedFile.access" state path $ do
  let rpath = realPath state path
      (readflag,writeflag,execflag) = accessFlags perms
  ok <- fileAccess rpath readflag writeflag execflag
  when (not ok) (throwIO (SException "Access check failed" (Just path) ePERM))
