{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Cabal where

import Data.Maybe
import           Data.Monoid
import           Control.Exception
import Control.Monad (mapM)
import System.Directory
import System.FilePath
import Data.List
import Data.Char

import           Distribution.Package
import           Distribution.PackageDescription hiding (Library, Benchmark, TestSuite, Executable)
import           Distribution.Simple.BuildTarget
import           Distribution.Simple.Configure
import           Distribution.Simple.LocalBuildInfo
import           Language.Haskell.Extension

data Target = Library
            | Executable String
            | TestSuite String
            | Benchmark String

showTarget Library = "library"
showTarget (Executable n) = "executable:" ++ n
showTarget (TestSuite n) = "test-suite:" ++ n
showTarget (Benchmark n) = "benchmark:" ++ n

mightExist :: FilePath -> IO (Maybe FilePath)
mightExist f = do
    exists <- doesDirectoryExist f
    return $ if exists then Just f else Nothing

-- | Get path to sandbox config file
getLbi :: IO (Maybe LocalBuildInfo)
getLbi =
  do currentDir <- getCurrentDirectory
     distDir <- mightExist (currentDir </> "dist")
     mapM getPersistBuildConfig distDir

extensionStrings :: LocalBuildInfo -> Target -> IO [String]
extensionStrings lbi@LocalBuildInfo{..} target =
  do buildTargets <- case target of
                       Library  -> return [BuildTargetComponent CLibName]
                       _otherwise -> readBuildTargets localPkgDescr [showTarget target]
     (comp, clbi, exts) <- case buildTargets of
                             []    -> throwIO $ userError "Invalid target"
                             _:_:_ -> throwIO $ userError "Ambiguous target"
                             [BuildTargetComponent component] -> do
                               let !comp = getComponent               localPkgDescr component
                                   !clbi = getComponentLocalBuildInfo lbi           component
                               exts <- getExtensions localPkgDescr (showTarget target)
                               return (comp, clbi, exts)
                             [BuildTargetModule _component _module] ->
                               throwIO $ userError "Unsupported target"
                             [BuildTargetFile _component _file] ->
                               throwIO $ userError "Unsupported target"
     return (showExtensions exts)

-- | Set GHC options.
showExtensions  :: [Extension] -> [String]
showExtensions = map showExt
  where showExt :: Extension -> String
        showExt g =
          case g of
            EnableExtension e -> "-X" <> show e
            DisableExtension e -> "-XNo" <> show e
            UnknownExtension e -> "-X" <> show e

-- | Resolve language extensions for a given buiold target
--
-- Loads the list of langauge extensions declared in the `extensions` or
-- `default-extensions` field of the cabal file for a given build target.
getExtensions :: PackageDescription -> String -> IO [Extension]
getExtensions PackageDescription{..} target =
  let (targetType, targetRest) = break (== ':') target
      _:targetName = targetRest
      targetBuildInfo =
        case targetType of
          "library" -> fmap libBuildInfo library
          "executable" -> fmap buildInfo (find ((== targetName) . exeName) executables)
          "test-suite" -> fmap testBuildInfo (find ((== targetName) . testName) testSuites)
          "benchmark"  -> fmap benchmarkBuildInfo (find ((== targetName) . benchmarkName) benchmarks)
          _  -> return mempty
  in case targetBuildInfo of
       Just !bi -> return $ usedExtensions bi
       Nothing  -> throwIO $ userError "Unsupported target"
