module System.SecretFS.RegularFile where

import qualified Data.ByteString.Char8 as BS

import System.Fuse(OpenMode,OpenFileFlags)
import System.IO(IOMode(..),SeekMode(..),openFile,hClose,hSeek)
import System.Posix.Types(ByteCount,FileOffset,EpochTime)
import System.Posix.Files(setFileSize)

import System.SecretFS.Core

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

regularFileSetSize :: State -> FilePath -> FileOffset -> IO ()
regularFileSetSize state path offset =  setFileSize (realPath state path) offset
  
  
