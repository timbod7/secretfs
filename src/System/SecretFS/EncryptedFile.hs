{-# LANGUAGE OverloadedStrings #-}
module System.SecretFS.EncryptedFile where

import qualified Data.ByteString.Char8 as BS

import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Crypto.RNCryptor.V3.Decrypt(decrypt)
import Crypto.RNCryptor.V3.Encrypt(encrypt)
import Crypto.RNCryptor.Types(newRNCryptorContext,newRNCryptorHeader)
import System.Directory(doesFileExist)
import System.Fuse(OpenMode,OpenFileFlags)
import System.IO(IOMode(..),SeekMode(..),openFile,hClose,hSeek)
import System.Posix.Types(ByteCount,FileOffset,EpochTime)

import System.SecretFS.Core

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
