{-# LANGUAGE OverloadedStrings #-}
module System.SecretFS.EncryptedFile(
  encryptedFileOps
  ) where

import qualified Data.ByteString.Char8 as BS

import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Crypto.RNCryptor.V3.Decrypt(decrypt)
import Crypto.RNCryptor.V3.Encrypt(encrypt)
import Crypto.RNCryptor.Types(newRNCryptorContext,newRNCryptorHeader)
import System.Directory(doesFileExist)
import System.Fuse(OpenMode,OpenFileFlags,FileStat(..),EntryType(..))
import System.IO(IOMode(..),SeekMode(..),openFile,hClose,hSeek)
import System.Posix.Types(ByteCount,FileOffset,EpochTime)
import System.Posix.Files(getFileStatus)

import System.SecretFS.Core

data EncFileState = EncFileState
  { efs_cleartext :: BS.ByteString
  , efs_dirty :: Bool
  }

encryptedFileOps :: State -> FilePath -> IO FileOps
encryptedFileOps state filepath = return FileOps{
  fo_open=encryptedFileOpen state filepath,
  fo_getFileStat=encryptedGetFileStat state filepath,
  fo_setFileSize=encryptedFileSetSize state filepath
  }

encryptedFileOpen :: State -> FilePath -> OpenMode -> OpenFileFlags -> IO SHandle
encryptedFileOpen state path _ _ = do
  exists <- doesFileExist rpath
  istate <- if exists
    then do
      clearText <- decryptReadFile rpath (s_keyPhrase state)
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
    readFile efstate byteCount offset = logcall "encryptedFile.read" state rpath $ do
      (EncFileState cleartext dirty) <- atomically (readTVar efstate)
      return (BS.take (fromIntegral byteCount) (BS.drop (fromIntegral offset) cleartext))

    writeFile :: TVar EncFileState -> BS.ByteString -> FileOffset -> IO ByteCount
    writeFile efstate content offset = logcall "encryptedFile.write" state path $ do
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
    flushFile efstate = logcall "encryptedFile.flush" state path $ do
      (EncFileState cleartext dirty) <- atomically (readTVar efstate)
      when dirty $ encryptWriteFile rpath (s_keyPhrase state) cleartext

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

encryptedFileSetSize :: State -> FilePath -> FileOffset -> IO ()
encryptedFileSetSize state path size =  do
  exists <- doesFileExist rpath
  when exists $ do
    clearText <- decryptReadFile rpath (s_keyPhrase state)
    when (BS.length clearText /= fromIntegral size) $ do
      let clearText' = BS.take (fromIntegral size) clearText
      encryptWriteFile rpath clearText' (s_keyPhrase state)
  where
    rpath = realPath state path

encryptedGetFileStat :: State -> FilePath -> IO FileStat
encryptedGetFileStat state path = do
  exists <- doesFileExist rpath
  if exists
    then do
      fileStat <- convertStatus <$> getFileStatus rpath
      -- Need some sort of caching for this
      clearTextLength <- BS.length <$> decryptReadFile rpath (s_keyPhrase state)
      return fileStat{statFileSize=fromIntegral clearTextLength}
      
    else return newFileStat
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

  
  
