{-# LANGUAGE OverloadedStrings #-}

module System.SecretFS.DirConfig where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M

import Control.Applicative
import Control.Exception(throwIO)
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Foreign.C.Error(eFAULT)
import System.FilePath((</>),takeDirectory,takeFileName)
import System.Directory(doesFileExist)
import System.Posix.Files(modificationTime, getFileStatus)

import System.SecretFS.Core

-- | Get the type of a single file
getFileType :: State -> FilePath ->IO MagicFileType
getFileType state path = do
  dc <- getDirConfig state (takeDirectory path)
  case M.lookup (takeFileName path) (dc_fileTypes dc) of
    Nothing -> return Regular
    (Just magicType) -> return magicType

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
       Nothing -> throwIO (SException "Invalid .secretfs file" (Just cpath) eFAULT)
    else do
      return (DirConfig M.empty)

-- Quick and dirty json parsing of the directory
-- config
instance JSON.FromJSON DirConfig where
  parseJSON o = DirConfig <$> JSON.parseJSON o

instance JSON.FromJSON MagicFileType where
  parseJSON (JSON.Object hm) = case HM.toList hm of
    [(k,v)] | k == "interpolated" -> Interpolated <$> JSON.parseJSON v
            | k == "regular" -> pure Regular
            | k == "encrypted" -> pure Encrypted
            | otherwise -> empty
              
  parseJSON _ = empty

