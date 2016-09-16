module System.SecretFS.InterpolatedFile where

import qualified Data.ByteString.Char8 as BS

import Control.Exception(throwIO)
import System.Fuse(OpenMode,OpenFileFlags)
import System.IO(IOMode(..),SeekMode(..),openFile,hClose,hSeek)
import System.Fuse

import System.SecretFS.Core


interpolatedFileOpen :: State -> FilePath -> OpenMode -> OpenFileFlags -> IO SHandle
interpolatedFileOpen state path ReadOnly _ = undefined
interpolatedFileOpen _ path _ _ = throwIO (SException "Interpolated files are readonly" (Just path) ePERM)

