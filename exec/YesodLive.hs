{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module YesodLive where

import           Cabal
import           Control.Concurrent
import           Control.Concurrent.Async (race)
import           Control.Concurrent.MVar
import qualified Control.Exception as Ex
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State (StateT, get, put, runStateT)
import           Control.Monad.Trans.Writer (WriterT, tell, execWriterT)
import           Data.IORef
import           Data.Maybe
import           DynFlags
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Filesystem.Path as FSP
import qualified Filesystem.Path.CurrentOS as FSP
import qualified Filesystem as FS
import           GHC
import           GHC.Paths
import           Linker
import           Outputable
import           Packages
import           SandboxPath
import           System.FSNotify
import           System.FilePath
import           Turtle.Prelude (touch)
import           YesodDeps

data Reload = JustReload | Reconfigure | ReloadExtra [FilePath]
  deriving (Show)

directoryWatcher :: [String] -> IO (Chan Reload, ThreadId)
directoryWatcher hsSourceDirs = do
    eventChan <- newChan
    dir <- fmap (FSP.</> "") FS.getWorkingDirectory
    let dirT =  dir
        rld = shouldReload dirT
    wid <- forkIO $ withManager $ \manager -> do
          -- start a watching job (in the background)
          watchBase <- watchTree manager dir rld $ \event -> do
            let fp = eventPath event
                (added, modified, deleted) =
                  case event of
                    Added _ _ -> (True, False, False)
                    Modified _ _ -> (False, True, False)
                    Removed _ _ -> (False, False, True)
            if | FSP.hasExtension fp "cabal" -> writeChan eventChan Reconfigure
               | FSP.hasExtension fp "hs" -> writeChan eventChan JustReload
               | True ->  do (_, deps) <- getDeps hsSourceDirs
                             let changes = runStateT (execWriterT (updatedDeps deps))
                             (depHsFiles, _) <- changes mempty
                             writeChan eventChan (ReloadExtra depHsFiles)
          -- Keep the watcher alive forever
          forever (threadDelay 1000000)

    return (eventChan, wid)

shouldReload :: FSP.FilePath -> Event -> Bool
shouldReload dir event = (not (or exclude) && or include)
  where fp = eventPath event
        p = T.toLower $ case FSP.toText fp of
                          Left filePath -> filePath
                          Right filePath -> filePath
        filename = T.toLower $ case FSP.toText (FSP.filename fp) of
                                 Left filePath -> filePath
                                 Right filePath -> filePath
        exclude = [ FSP.hasExtension fp ".tmp", inPath "tmp"
                  , inFile "#", inFile "flycheck_"
                  , inFile ".gitignore"]
        include = [ inRootPath "static", inRootPath "handler"
                  , inRootPath "templates", inRootPath "config"
                  , inRootPath "settings", dir == FSP.directory fp]
        baseDirT = T.toLower (stripPrefix (fpToText dir) p)
        fpToText :: FSP.FilePath -> Text
        fpToText path = T.toLower (either id id (FSP.toText path))
        inPath t = t `T.isInfixOf` baseDirT
        inRootPath t = t `T.isInfixOf` baseDirT
        inFile t = t `T.isInfixOf` filename
        stripPrefix pre t = fromMaybe t (T.stripPrefix pre t)


recompiler :: FilePath -> [FilePath] -> IO ()
recompiler mainFileName importPaths' = withGHCSession mainFileName importPaths' $ do
    {-
    Watcher:
        Tell the main thread to recompile.
        If the main thread isn't done yet, kill it.
    Compiler:
        Wait for the signal to recompile.
        Before recompiling & running, mark that we've started,
        and after we're done running, mark that we're done.
    -}
    mainDone  <- liftIO $ newIORef False
    -- Start with a full MVar so we recompile right away.
    recompile <- liftIO $ newMVar ()
    reconfigure <- liftIO $ newIORef False
    mainThreadId <- liftIO myThreadId
    (hsSourceDirs, _) <- liftIO checkCabalFile

    -- Watch for changes and recompile whenever they occur
    wc <- liftIO (directoryWatcher hsSourceDirs)
    watcherRef  <- liftIO $ newIORef wc
    _ <- liftIO . forkIO . forever $ do
        (watcher, wid) <- readIORef watcherRef
        e <- readChan watcher
        print e
        case e of
          JustReload -> return ()
          Reconfigure -> writeIORef reconfigure True
          ReloadExtra depHsFiles -> do
            killThread wid
            mapM_ (touch . FSP.decodeString) depHsFiles
            threadDelay 100000 -- 0.1s
            directoryWatcher hsSourceDirs >>= writeIORef watcherRef
        putMVar recompile ()
        mainIsDone <- readIORef mainDone
        unless mainIsDone $ killThread mainThreadId

    -- Start up the app
    let loop = do _ <- liftIO $ takeMVar recompile
                  liftIO $ writeIORef mainDone False
                  liftIO $ putStrLn "recompiling"
                  recompileTargets
                  liftIO $ writeIORef mainDone True
                  shouldReconfigure <- liftIO $ readIORef reconfigure
                  unless shouldReconfigure loop
    loop

withGHCSession :: FilePath -> [FilePath] -> Ghc () -> IO ()
withGHCSession mainFileName importPaths' action = do
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ runGhc (Just libdir) $ do
        -- Add the main file's path to the import path list
        let mainFilePath = dropFileName mainFileName
            importPaths'' = mainFilePath:importPaths'

        -- Get the default dynFlags
        dflags0 <- getSessionDynFlags

        -- If there's a sandbox, add its package DB
        dflags1 <- liftIO getSandboxDb >>= \case
            Nothing -> return dflags0
            Just sandboxDB -> do
                let pkgs = map PkgConfFile [sandboxDB]
                return dflags0 { extraPkgConfs = (pkgs ++) . extraPkgConfs dflags0 }

        dflags2 <- (liftIO getLbi) >>= \case
            Nothing -> return dflags1
            Just lbi ->do extensions <- liftIO $ extensionStrings lbi Library
                          (df, _, _) <- parseDynamicFlags dflags1 (map noLoc extensions)
                          return df

        -- Make sure we're configured for live-reload, and turn off the GHCi sandbox
        -- since it breaks OpenGL/GUI usage
        let dflags3 = dflags2 { hscTarget = HscInterpreted
                              , ghcLink   = LinkInMemory
                              , ghcMode   = CompManager
                              , importPaths = importPaths''
                              } `gopt_unset` Opt_GhciSandbox

        -- We must set dynflags before calling initPackages or any other GHC API
        _ <- setSessionDynFlags dflags3

        -- Initialize the package database
        (dflags4, _) <- liftIO $ initPackages dflags3

        -- Initialize the dynamic linker
        liftIO $ initDynLinker dflags4

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
        RunException exception -> liftIO $ putStrLn ("Runner exception:"  ++ show exception)
        RunBreak _ _ _ -> liftIO $ putStrLn "Breakpoint"


-- A helper from interactive-diagrams to print out GHC API values,
-- useful while debugging the API.
-- | Outputs any value that can be pretty-printed using the default style
output :: (GhcMonad m, MonadIO m) => Outputable a => a -> m ()
output a = do
    dfs <- getSessionDynFlags
    let style = defaultUserStyle
    let cntx  = initSDocContext dfs style
    liftIO $ print $ runSDoc (ppr a) cntx
