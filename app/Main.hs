module Main where

import System.Fuse(fuseMain,defaultExceptionHandler)
import System.SecretFS

main :: IO ()
main = fuseMain secretFSOps defaultExceptionHandler
