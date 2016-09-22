{-# LANGUAGE OverloadedStrings #-}
module System.SecretFS.RegularFile(
  regularFileOps
  ) where

import qualified Data.ByteString.Char8 as BS

import Data.Monoid
import System.Fuse(OpenMode,OpenFileFlags,FileStat,EntryType(..),entryTypeToFileMode,unionFileModes)
import System.IO(IOMode(..),SeekMode(..),openFile,hClose,hSeek)
import System.Posix.Types(ByteCount,FileOffset,EpochTime,FileMode,DeviceID)
import System.Posix.Files(setFileSize,getFileStatus,createDevice)

import System.SecretFS.Core

regularFileOps :: State -> FilePath -> IO FileOps
regularFileOps state filepath = return FileOps{
  fo_open=regularFileOpen state filepath,
  fo_getFileStat=regularFileGetStat state filepath,
  fo_setFileSize=regularFileSetSize state filepath,
  fo_createDevice=regularFileCreateDevice state filepath
  }

regularFileOpen :: State -> FilePath -> OpenMode -> OpenFileFlags -> IO SHandle
regularFileOpen state path mode flags = logcall "regularFile" state path $ do
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

regularFileGetStat :: State -> FilePath -> IO FileStat
regularFileGetStat state path = convertStatus <$> getFileStatus (realPath state path)

regularFileSetSize :: State -> FilePath -> FileOffset -> IO ()
regularFileSetSize state path offset =  setFileSize (realPath state path) offset
  
regularFileCreateDevice :: State -> FilePath ->  EntryType -> FileMode -> DeviceID -> IO ()
regularFileCreateDevice state path entryType fileMode deviceID = logcall "regularFile.createDevice" state path $ do
  case entryType of
    RegularFile -> do
      -- on OSX, it seems that mknod needs root access even for regular files.
      -- So create regular files with an op/close combination.
      openFile (realPath state path) WriteMode >>= hClose
    _ -> do
      let fileMode' = entryTypeToFileMode entryType `unionFileModes` fileMode
      createDevice (realPath state path) fileMode' deviceID
