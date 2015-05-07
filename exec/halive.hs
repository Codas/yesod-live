{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Halive where

import DynFlags
import GHC
-- import Outputable
import Linker
import Packages
import GHC.Paths
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Monad
import SandboxPath
import System.FSNotify
import qualified Filesystem.Path as FSP

directoryWatcher :: IO (Chan Event)
directoryWatcher = do
    let predicate event = case event of
            Modified path _ -> FSP.extension path == Just "hs"
            _               -> False
    eventChan <- newChan
    _ <- forkIO $ withManager $ \manager -> do
        -- start a watching job (in the background)
        let watchDirectory = "."
        _stopListening <- watchTreeChan
            manager
            watchDirectory
            predicate
            eventChan
        -- Keep the watcher alive forever
        forever $ threadDelay 10000000

    return eventChan

recompiler :: FilePath -> [FilePath] -> IO ()
recompiler mainFileName importPaths' = withGHCSession mainFileName importPaths' $ do
    mainThreadId <- liftIO myThreadId

    -- Watch for changes and recompile whenever they occur
    watcher <- liftIO directoryWatcher
    _ <- liftIO . forkIO . forever $ do
        _ <- readChan watcher
        killThread mainThreadId
    
    -- Start up the app
    forever $ do
        recompileTargets


withGHCSession :: FilePath -> [FilePath] -> Ghc () -> IO ()
withGHCSession mainFileName importPaths' action = do
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ runGhc (Just libdir) $ do
        
        -- Get the default dynFlags
        dflags0 <- getSessionDynFlags
        
        -- If there's a sandbox, add its package DB
        dflags1 <- liftIO getSandboxDb >>= \case
            Nothing -> return dflags0
            Just sandboxDB -> do
                let pkgs = map PkgConfFile [sandboxDB]
                return dflags0 { extraPkgConfs = (pkgs ++) . extraPkgConfs dflags0 }

        -- Make sure we're configured for live-reload, and turn off the GHCi sandbox
        -- since it breaks OpenGL/GUI usage
        let dflags2 = dflags1 { hscTarget = HscInterpreted
                              , ghcLink   = LinkInMemory
                              , ghcMode   = CompManager
                              , importPaths = importPaths'
                              } `gopt_unset` Opt_GhciSandbox
        
        -- We must set dynflags before calling initPackages or any other GHC API
        _ <- setSessionDynFlags dflags2

        -- Initialize the package database
        (dflags3, _) <- liftIO $ initPackages dflags2

        -- Initialize the dynamic linker
        liftIO $ initDynLinker dflags3 

        -- Set the given filename as a compilation target
        setTargets =<< sequence [guessTarget mainFileName Nothing]

        action

-- Recompiles the current targets
recompileTargets :: Ghc ()
recompileTargets = handleSourceError printException $ do
    graph <- depanal [] False

    _success <- load LoadAllTargets

    -- We must parse and typecheck modules before they'll be available for usage
    forM_ graph (typecheckModule <=< parseModule)
    
    setContext $ map (IIModule . ms_mod_name) graph

    rr <- runStmt "main" RunToCompletion
    case rr of
        RunOk _ -> liftIO $ putStrLn "OK"
        _ -> liftIO $ putStrLn "Error :*("