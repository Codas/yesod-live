{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
import           YesodLive
import           Banner
import           System.Environment
import           Control.Concurrent.MVar
import           Control.Concurrent
import           Control.Monad
import           System.IO
import qualified Control.Exception as Ex
import           System.Process (ProcessHandle, createProcess
                                ,env, getProcessExitCode, proc
                                ,readProcess, system, terminateProcess)
import           System.Exit (ExitCode (..), exitFailure, exitSuccess)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Usage: halive <main.hs> <include dir>"
        (mainName:includeDirs) -> do
            putStrLn banner
            forever $ do
              recompiler mainName includeDirs
              putStrLn "Reconfigure. Using cabal"
              configure

configure :: IO Bool
configure =
    checkExit =<< createProcess (proc "cabal"
                                   [ "configure"
                                   , "-flibrary-only"
                                   , "--disable-tests"
                                   , "--disable-benchmarks"
                                   , "-fdevel"
                                   , "--disable-library-profiling"
                                   , "--with-ld=yesod-ld-wrapper"
                                   , "--with-ghc=yesod-ghc-wrapper"
                                   , "--with-ar=yesod-ar-wrapper"
                                   , "--with-hc-pkg=ghc-pkg"
                                   ])

-- | nonblocking version of @waitForProcess@
waitForProcess' :: ProcessHandle -> IO ExitCode
waitForProcess' pid = go
  where
    go = do
      mec <- getProcessExitCode pid
      case mec of
        Just ec -> return ec
        Nothing -> threadDelay 100000 >> go

-- | wait for process started by @createProcess@, return True for ExitSuccess
checkExit :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO Bool
checkExit (_,_,_,h) = (==ExitSuccess) <$> waitForProcess' h
