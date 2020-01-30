{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Platform.Packaging.PythonImports.Internal.PipMatches where

import           "base"                     Control.Monad.IO.Class (MonadIO, liftIO)
import           "base"                     Control.Monad (join, when, unless, filterM, zipWithM, sequence_)
import           "base"                     Data.Typeable (Typeable)
import           "base"                     Data.Data (Data)
import           "base"                     Data.List (foldl', intercalate, isSuffixOf, partition, sortBy, (\\))
import           "base"                     Data.Maybe (isJust, fromJust)
import           "base"                     GHC.Generics (Generic)
import           "aeson"                    Data.Aeson (FromJSON, ToJSON, toEncoding, genericToEncoding, defaultOptions)
import           "data-default"             Data.Default (def)
import qualified "fuzzyset"                 Data.FuzzySet as FS (fromList, get)
import           "text"                     Data.Text (Text, pack, unpack, replace, split)
import qualified "text"                     Data.Text as T
import qualified "text"                     Data.Text.IO as TIO
import           "extra"                    Data.Tuple.Extra ((&&&))
import           "extra"                    Control.Monad.Extra (concatMapM)
import           "lens"                     Control.Lens
import           "exceptions"               Control.Monad.Catch (Exception, MonadMask, MonadCatch, MonadThrow, throwM, catchAll, handleAll, catchIOError, bracket, catchIf)
import           "regex-pcre"               Text.Regex.PCRE
import           "directory"                System.Directory (listDirectory, doesDirectoryExist, doesFileExist, getTemporaryDirectory, setCurrentDirectory, getCurrentDirectory, createDirectoryIfMissing, removeFile, removeDirectoryRecursive, canonicalizePath)
import           "temporary"                System.IO.Temp (createTempDirectory)
import           "language-python"          Language.Python.Common.AST
import           "language-python"          Language.Python.Common.Token (Token, token_literal, token_span)
import           "language-python"          Language.Python.Common.SrcLocation (SrcSpan(SpanCoLinear))
import           "language-python"          Language.Python.Version3.Parser (parseModule, parseStmt)
import           "language-python"          Language.Python.Common.ParseError (ParseError)
import           "shellmet"                 Shellmet
import           "platform-packaging-types" Platform.Packaging.Pip.Types
import           "platform-packaging-types" Platform.Packaging.PythonImports.Types
import                                      Platform.Packaging.PythonImports.Internal.Utils
import                                      Platform.Packaging.Pip

makeLenses ''GeneralOpts
makeLenses ''DownloadOpts
makeLenses ''PipInput

searchAndListNames :: (MonadCatch m, MonadIO m)
                   => Text
                   -> m [Text]
searchAndListNames pkg = do
    let firstWordRegex = "^[^ ]+(?= )" :: String
    t <- catchAll (liftIO . search def def $ pkg) (const $ return "")
    let matchSet = FS.fromList
                 . map pack
                 $ getAllTextMatches (unpack t =~ firstWordRegex :: AllTextMatches [] String)
    let rankedMatches = map snd
                      . sortBy comparePair
                      . FS.get matchSet
                      $ pkg
    return rankedMatches
    where
        comparePair a b = compare (fst a) (fst b)

findPossibleMatches :: (MonadCatch m, MonadIO m)
                    => ModuleName
                    -> m [PyPkg]
findPossibleMatches mn = do
    pkgs <- fmap concat
          . sequence
          . map searchAndListNames
          $ concatenatedSubNames
    return $ map pypiPkg pkgs
    where
        concatenatedSubNames = map (T.intercalate "-" . flip take subNames) [1..length subNames]
        subNames             = split (== '.') . _moduleName $ mn

findMatch :: (MonadMask m, MonadIO m)
          => ModuleName
          -> [PyPkg]
          -> m (Maybe PyPkg)
findMatch mn pkgs = do
    candidates <- filterM filterCondition pkgs
    if null candidates
       then return Nothing
       else return . return . head $ candidates
    where
        filterCondition pkg = handleAll (\_ -> return False) (fmap (== PkgContainsModule) $ isModuleInPkg mn pkg)

-- for TopLevel.MidLevel.ModName, this would guess
--   "TopLevel" > "TopLevel MidLevel" > "TopLevel MidLevel ModName"
-- > "MidLevel" > "MidLevel ModName" > "ModName"
-- Perhaps there are better guessing orders, but this one seems pretty reasonable.
pkgGuesses :: DottedName annot -> [Text]
pkgGuesses = map (pack . unwords) . supLevelSets . map ident_string
    where supLevelSets (x:xs) = (x:xs) : supLevelSets xs
          supLevelSets [] = []

-- DottedName annot = [Ident annot]
-- [Ident annot] -> [String] -> String -> Text

isModuleInPkg :: (MonadMask m, MonadIO m)
              => ModuleName
              -> PyPkg
              -> m PkgVerdict
isModuleInPkg modName pkg = handleAll (const . return $ Inconclusive) $
    bracket init'
            fini
            body
    where
        init' = do
            originalDir <- liftIO getCurrentDirectory
            tmp <- liftIO getTemporaryDirectory
            dlDir <- liftIO . createTempDirectory tmp $ (unpack . _pyPkg_name $ pkg)
            liftIO $ setCurrentDirectory dlDir
            return (originalDir, dlDir)

        fini (originalDir, dlDir) = do
            liftIO . removeDirectoryRecursive $ dlDir
            liftIO $ setCurrentDirectory originalDir

        body (_, dlDir) = do
            downloadPkg pkg
            downloadedPkg <- liftIO $ headIfLengthIsOne =<< listDirectory dlDir
            pkgType <- determinePkgType downloadedPkg
            modulesInPkg <- if pkgType == TarPackage
                               then genericWrapper tarAction dlDir
                               else genericWrapper wheelAction dlDir
            if modName `elem` modulesInPkg
               then return PkgContainsModule
               else return PkgLacksModule

        determinePkgType fp = do
            let isTar   = fp =~ tarRegex
            let isWheel = fp =~ wheelRegex
            unless (isTar || isWheel) (throwM ImpossibleFileState)
            when   (isTar && isWheel) (throwM ImpossibleFileState)
            if isTar
               then return TarPackage
               else return WheelPackage

downloadPkg :: (MonadCatch m, MonadIO m)
            => PyPkg
            -> m ()
downloadPkg pkg = do
    liftIO $ download genOpts dlOpts dlInput
    return ()
    where
        genOpts = set generalOpts_quiet   (Just True) def
        dlOpts =  set downloadOpts_noDeps (Just True) def
        dlInput = ReqSpecInput [ReqSpec (_pyPkg_name pkg) Nothing]
             --  set downloadOpts_index  (Just $ _pyPkg_index pkg) . set downloadOpts_noDeps (Just True) $ def

getPythonFileNameRegex :: String
getPythonFileNameRegex = "[^\\/]+(?=.py$)"

tarRegex :: String
tarRegex = "\\.tar\\.gz$"

wheelRegex :: String
wheelRegex = "\\.whl"

-- genericWrapper gives the modules exported by a package.
-- This function should be provided the filepath to a folder
-- containing a single .whl file.
genericWrapper :: (MonadMask m, MonadIO m)
               => (FilePath -> m [ModuleName])
               -> FilePath
               -> m [ModuleName]
genericWrapper action fp = do
    bracket init'
            fini
            body
    where
        init' = do
            dir <- liftIO $ getCurrentDirectory
            contents <- liftIO $ listDirectory fp
            return (dir, contents)
        fini (dir, originalContents) = do
            modifiedContents <- liftIO $ listDirectory fp
            let newElements = filter (`notElem` originalContents) modifiedContents
            sequence_ $ map removeEntity newElements
            liftIO $ setCurrentDirectory dir
        removeEntity entity = do
            isDir <- liftIO $ doesDirectoryExist entity
            isFile <- liftIO $ doesFileExist entity
            unless (isDir || isFile) (throwM ImpossibleFileState)
            when   (isDir && isFile) (throwM ImpossibleFileState)
            if isDir
               then liftIO $ removeDirectoryRecursive entity
               else liftIO $ removeFile entity
        body _ = do
            liftIO $ setCurrentDirectory fp
            action fp

wheelAction :: (MonadCatch m, MonadIO m)
            => FilePath
            -> m [ModuleName]
wheelAction fp = do
    originalContents <- liftIO $ listDirectory fp
    let wheelFiles = filter (=~ wheelRegex) originalContents
    wheelFile <- headIfLengthIsOne wheelFiles
    liftIO $ "unzip" $| [pack wheelFile]
    newContents <- liftIO
                  . fmap (filter (`notElem` originalContents))
                  . listDirectory
                  $ fp
    subdirs <- liftIO $ filterM doesDirectoryExist newContents
    let distDirs = filter (=~ ("\\.dist-info/?$" :: String)) subdirs
    distDir <- headIfLengthIsOne distDirs
    getModuleNames (distDir <> "/top_level.txt") fp

getModuleNames :: (MonadCatch m, MonadIO m)
               => FilePath -- ^ the location of the `top_level.txt` file
               -> FilePath -- ^ prefix for the top level module paths
               -> m [ModuleName]
getModuleNames topLevelFile prefix = do
    topLevelFileExists <- liftIO . doesFileExist $ topLevelFile
    unless topLevelFileExists (throwM NoTopLevelFile)
    topLevelModuleNames <- liftIO
                         . fmap T.lines
                         . TIO.readFile
                         $ topLevelFile
    let topLevelModules     = map ModuleName                      topLevelModuleNames
        topLevelModulePaths = map (\mn -> prefix <> "/" <> unpack mn) topLevelModuleNames
    subModules <- fmap concat
                . sequence
                $ map (enumerateSubmodules (prefix <> "/")) topLevelModulePaths
    return $ topLevelModules <> subModules

-- Look for package_dir and modules in two places.
-- First check in setup.setup()'s kwargs if present, then in setup.cfg.
-- If this doesn't work then throw an error; it's not runPythonImport's job to run the build
-- process (which involves arbitrary code) just to find out what modules a package
-- exports.
tarAction :: (MonadCatch m, MonadIO m)
             => FilePath
             -> m [ModuleName]
tarAction fp = do
    originalContents <- liftIO $ listDirectory fp
    let tarFiles = filter (=~ tarRegex) originalContents
    tarFile <- headIfLengthIsOne tarFiles
    liftIO $ "tar" $| ["-xzf", pack tarFile]
    newContents <- liftIO
                 . fmap (filter (`notElem` originalContents))
                 . listDirectory
                 $ fp
    subdirs <- liftIO $ filterM doesDirectoryExist newContents
    eggInfoDirs <- liftIO
                 . fmap concat
                 . sequence
                 . map (\dir -> fmap (filter (not . null)
                                    . lines
                                    . unpack)
                              $ "find" $| [pack dir, "-regex", ".*egg-info"])
                 $ subdirs
    eggInfoDir <- headIfLengthIsOne eggInfoDirs
    dirAboveEggInfoDir <- liftIO -- TODO this is probably an error; need to delete .egg-info from eggInfoDir instead
                        . canonicalizePath
                        $ eggInfoDir <> "/.."
    getModuleNames (eggInfoDir <> "/top_level.txt") undefined
