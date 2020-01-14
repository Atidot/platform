{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Platform.Packaging.PythonImports
    ( ModuleName
    , PyPkg(..)
    , PythonImportException
    , runPythonImports
    , getAST
    ) where

import           "base"                     Debug.Trace (trace)
import           "base"                     Control.Monad.IO.Class (MonadIO, liftIO)
import           "base"                     Control.Monad (when, unless, filterM, zipWithM, sequence_)
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
import                                      Platform.Packaging.Pip

type URL = Text

data PyPkg
    = PyPkg
    { _pyPkg_index :: !URL
    , _pyPkg_name :: !Text
    } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance ToJSON PyPkg where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON PyPkg where

data ModuleName = ModuleName { _moduleName :: !Text }
    deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance ToJSON ModuleName where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ModuleName where

data PythonImportException
    = ModuleInNoPackages
    | FileNotParseable
    | ImpossibleFileState
    | NoInitFile
    | NoTopLevelFile
    | NoSuchFile
    | MultipleSuchFiles
    deriving (Read, Eq, Ord, Bounded, Enum, Data, Typeable, Generic)

instance Show PythonImportException where
    show ModuleInNoPackages = "No package was found exporting the appropriate module name."
    show FileNotParseable = "This file could not be parsed as Python 3 code."

instance Exception PythonImportException

data PackageFormat
    = TarPackage
    | WheelPackage
    deriving (Read, Eq, Ord, Bounded, Enum, Data, Typeable, Generic)

data PkgVerdict
    = PkgContainsModule
    | PkgLacksModule
    | Inconclusive
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable, Data, Generic)

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

headIfLengthIsOne :: (MonadThrow m, MonadIO m)
                  => [a]
                  -> m a
headIfLengthIsOne xs = do
    when (length xs == 0) (throwM NoSuchFile)
    when (length xs >= 2) (throwM MultipleSuchFiles)
    return $ head xs

pypiPkg :: Text -> PyPkg
pypiPkg = PyPkg "https://pypi.org/simple/"

doParse :: (MonadThrow m, MonadIO m)
        => String
        -> m (Module SrcSpan, [Token])
doParse contents = do
    let parsed = parseModule contents "" -- We don't use last arg so leave it empty
    return' parsed
    where return' (Right p) = return p
          return' (Left _) = throwM FileNotParseable

getAST :: (MonadThrow m, MonadIO m)
       => String
       -> m (Module SrcSpan)
getAST = fmap fst . doParse

getComments :: (MonadThrow m, MonadIO m)
            => String
            -> m [Token]
getComments = fmap snd . doParse

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
    candidates <- trace ("finding match for" <> show mn) (filterM filterCondition pkgs)
    if null candidates
       then trace ("failed to match on " <> show mn) (return Nothing)
       else trace ("found match for " <> show mn <> ": " <> show (head candidates)) (return . return . head $ candidates)
    where
        filterCondition pkg = handleAll (\_ -> return False) (fmap (== PkgContainsModule) $ isModuleInPkg mn pkg)

-- Non-recursive top-level search.
-- The Maybe is here to fit the language-python API
foreignImportedModules :: Statement annot -> [Maybe (DottedName annot)]
foreignImportedModules i@Import{}     = map (return . import_item_name)
                                      . import_items
                                      $ i
foreignImportedModules i@FromImport{} = if 0 == (import_relative_dots $ from_module i)
                                          then return
                                             . import_relative_module
                                             . from_module
                                             $ i
                                          else []
foreignImportedModules _              = []

-- Non-recursive top-level search.
-- The Maybe is here to fit the language-python API
relativeImportedModules :: Statement annot -> [Maybe (DottedName annot)]
relativeImportedModules i@FromImport{} = if 0 == (import_relative_dots $ from_module i)
                                            then []
                                            else return
                                               . import_relative_module
                                               . from_module
                                               $ i
relativeImportedModules _              = []

onlyJust :: [Maybe a] -> [a]
onlyJust (Nothing : xs) = onlyJust xs
onlyJust (Just x : xs)  = x : onlyJust xs
onlyJust []             = []

-- This takes a function like relativeImportedModules and returns a recursive search function.
getImportNamesConditional :: (Statement annot -> [Maybe (DottedName annot)])
                          -> [Statement annot]
                          -> [DottedName annot]
getImportNamesConditional conditional statements = onlyJust
                                                 . concatMap getImports'
                                                 $ statements
  where getImports' i@Import{}      = conditional i
        getImports' f@FromImport{}  = conditional f
        getImports' w@While{}       = recurse . (while_body <> while_else) $ w
        getImports' f@For  {}       = recurse . (for_body <> for_else) $ f
        getImports' a@AsyncFor{}    = recurse . return . for_stmt $ a
        getImports' f@Fun{}         = recurse . fun_body $ f
        getImports' a@AsyncFun{}    = recurse . return . fun_def $ a
        getImports' c@Class{}       = recurse . class_body $ c
        getImports' c@Conditional{} = recurse . concat . ((map snd . cond_guards) <> (return . cond_else)) $ c
        getImports' d@Decorated{}   = recurse . return . decorated_def $ d
        getImports' t@Try{}         = recurse . (try_body <> try_else <> try_finally) $ t
        getImports' w@With{}        = recurse . with_body $ w
        getImports' a@AsyncWith{}   = recurse . return . with_stmt $ a
        getImports' _               = []
        recurse = concatMap getImports'

-- Convenience function for reading whole module ASTs
getForeignImportNames :: Module annot -> [DottedName annot]
getForeignImportNames = getImportNamesConditional foreignImportedModules
                      . \(Module stmts) -> stmts

-- Convenience function for reading whole module ASTs
getRelativeImportNames :: Module annot -> [DottedName annot]
getRelativeImportNames = getImportNamesConditional relativeImportedModules
                       . \(Module stmts) -> stmts

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
dottedToModuleName :: DottedName annot -> ModuleName
dottedToModuleName dn = ModuleName
                      $ pack
                      . intercalate "."
                      . dropWhile (== mempty)
                      . map ident_string
                      $ dn

-- The output tuple of runPythonImports works as follows:
-- The first list is module names paired with their package names.
-- The second list is module names that could not be paired.
runPythonImports :: (MonadMask m, MonadIO m)
                 => String
                 -> m ([(ModuleName, PyPkg)], [ModuleName])
runPythonImports fileContents = do
    unmatchedMods <- map dottedToModuleName
                   . getForeignImportNames
                 <$> getAST fileContents
    explicitMatches <- getExplicitPkgOrigins fileContents
    let unmatchedMods' = unmatchedMods \\ map fst explicitMatches
    possibleMatches <- mapM findPossibleMatches unmatchedMods'
    let possibleMatchesTruncated = map (take 10) possibleMatches --TODO: calibrate this
    discoveredMatches <- zipWithM findMatch' unmatchedMods' possibleMatchesTruncated
    let ms                = partition (isJust . snd) discoveredMatches
    let successfulMatches = map (\(mn, pkg) -> (mn, fromJust pkg)) $ fst ms
    let failedMatches     = map (\(mn, _) -> mn)                   $ snd ms
    return (explicitMatches <> successfulMatches, failedMatches)
    where
        findMatch' unmatched possiblePkgs = do
            output <- findMatch unmatched possiblePkgs
            return (unmatched, output)

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
            trace ("downloaded package " ++ show downloadedPkg) (return ())
            pkgType <- determinePkgType downloadedPkg
            modulesInPkg <- if pkgType == TarPackage
                               then introspectWrapper tarAction dlDir
                               else introspectWrapper wheelAction dlDir
            if modName `elem` modulesInPkg
               then trace "declaring that the package contains the module" (return PkgContainsModule)
               else trace "declaring that the package lacks the module" (return PkgLacksModule)

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

getExplicitPkgOrigins :: (MonadThrow m, MonadIO m)
                      => String
                      -> m [(ModuleName, PyPkg)]
getExplicitPkgOrigins fileContents = do
    comments <- getComments fileContents
    let annotationFiltered = filter hasPkgAnnotation comments
    explicitPkgs <- filterM comesAfterImport annotationFiltered
    fmap (map (\(dn, pkg) -> (dottedToModuleName dn, pkg))) $ mapM processPkgs explicitPkgs
    where
        processPkgs :: (MonadThrow m, MonadIO m) => Token -> m (DottedName SrcSpan, PyPkg)
        processPkgs tkn = do
            stmt <- stmtBeforeComment tkn
            processed <- case stmt of
                           Nothing   -> throwM FileNotParseable
                           Just stmt -> headIfLengthIsOne
                                      . getForeignImportNames
                                      . Module
                                      $ [stmt]
            trace ("found that " <> show processed <> " has pkg token " <> show tkn) (return ())
            return (processed, pkgFromToken tkn)
        comesAfterImport :: (MonadThrow m, MonadIO m) => Token -> m Bool
        comesAfterImport tkn = do
            stmt <- stmtBeforeComment tkn
            return . isImport $ stmt
        hasPkgAnnotation :: Token -> Bool
        hasPkgAnnotation tkn
          = (token_literal tkn) =~ explicitPkgRegex
         || (token_literal tkn) =~ suppressPkgRegex
        stmtBeforeComment :: (MonadThrow m, MonadIO m) => Token -> m (Maybe (Statement SrcSpan))
        stmtBeforeComment tkn = do
            file <- fileFromCommentLine tkn
            case parseStmt file "" of
              Left _                  -> return Nothing
              Right ([], _)           -> return Nothing
              Right (stmt : stmts, _) -> return (Just stmt)
        fileFromCommentLine :: (MonadThrow m, MonadIO m) => Token -> m String
        fileFromCommentLine tkn = do
            beginning <- fmap (+(-1)) . commentLine . token_span $ tkn
            return . unlines . drop beginning . lines $ fileContents
        isImport :: Maybe (Statement annot) -> Bool
        isImport Nothing             = False
        isImport (Just Import{})     = True
        isImport (Just FromImport{}) = True
        commentLine :: (MonadThrow m, MonadIO m) => SrcSpan -> m Int
        commentLine (SpanCoLinear _ row _ _) = return row
        commentLine _                        = throwM FileNotParseable

explicitPkgRegex :: String
explicitPkgRegex = "(?<=!platform )(\\w|\\d)(\\w|\\d|-)*(?=( |\\t)*$)"

-- This matches on a `!platform`-prepended comment but does not capture anything.
suppressPkgRegex :: String
suppressPkgRegex = "(?<=!platform)(?=( |\\t)*$)"

getPythonFileNameRegex :: String
getPythonFileNameRegex = "[^\\/]+(?=.py$)"

tarRegex :: String
tarRegex = "\\.tar\\.gz$"

wheelRegex :: String
wheelRegex = "\\.whl"

pkgFromToken :: Token -> PyPkg
pkgFromToken tkn = PyPkg "pypi.org/simple" $ pack (token_literal tkn =~ explicitPkgRegex)

-- introspectWrapper gives the modules exported by a package.
-- This function should be provided the filepath to a folder
-- containing a single .whl file.
introspectWrapper :: (MonadMask m, MonadIO m)
                  => (FilePath -> m [ModuleName])
                  -> FilePath
                  -> m [ModuleName]
introspectWrapper action fp = do
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
    topLevelFileExists <- liftIO . doesFileExist $ distDir <> "/top_level.txt"
    unless topLevelFileExists (throwM NoTopLevelFile)
    topLevelModuleNames <- liftIO
                         . fmap T.lines
                         . TIO.readFile
                         $ distDir <> "/top_level.txt"
    let topLevelModules     = map ModuleName                      topLevelModuleNames
    let topLevelModulePaths = map (\mn -> fp <> "/" <> unpack mn) topLevelModuleNames
    subModules <- fmap concat
                . sequence
                $ map (enumerateSubmodules (fp <> "/")) topLevelModulePaths
    trace ("modules are: " <> show (topLevelModules <> subModules)) (return $ topLevelModules <> subModules)

-- TODO: debug this. Lower priority since most packages are in wheel format.
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
    topLevelModules <- liftIO
                     . fmap (map ModuleName . T.lines)
                     . TIO.readFile
                     $ eggInfoDir <> "/top_level.txt"
    trace ("got these toplevel modules: " <> show topLevelModules) (return ())
    out <- undefined
    --                     fmap concat . sequence
    --                   . map (enumerateSubmodules dirAboveEggInfoDir)
    --                   $ topLevelModules
    trace ("completed a tar action in " <> show fp) (return out)

containsInit :: (MonadThrow m, MonadIO m)
            => FilePath
            -> m Bool
containsInit fp = liftIO . doesFileExist $ fp <> "/__init__.py"

enumerateSubmodules :: (MonadCatch m, MonadIO m)
                    => FilePath -- The common prefix to be deleted
                    -> FilePath -- The location of the module
                    -> m [ModuleName]
enumerateSubmodules root fp = do
    initPresent <- containsInit fp
    trace ("init present is considered " <> show initPresent) (return ())
    unless initPresent (throwM NoInitFile)
    dirContents <- liftIO
                 . fmap (map (\f -> fp <> "/" <> f))
                 . listDirectory
                 $ fp
    subDirs <- liftIO $ filterM doesDirectoryExist dirContents
    files <- liftIO $ filterM doesFileExist dirContents
    let pyFiles = filter (isSuffixOf ".py") files
    let immediateSubmods = map moduleNameFromFilePath pyFiles
    additionalSubmods <- fmap concat
                       . sequence
                       $ map enumerateASubDir subDirs
    return $ immediateSubmods <> additionalSubmods
    where
        enumerateASubDir subDir = catchIf (== NoInitFile)
                                          (enumerateSubmodules root subDir)
                                          (const . return $ [])
        moduleNameFromFilePath fp = ModuleName
                                  . replace "/" "."
                                  . pack
                                  $ fp =~ ("(?<=" <> root <> ").*(?=.py$)" :: String)

readInit :: (MonadThrow m, MonadIO m)
         => FilePath
         -> m [(ModuleName, FilePath)]
readInit fp = do
    initAST <- getAST fp
    let rels = getRelativeImportNames initAST
    let locations = map findLocation $ rels
    return $ zip (map dottedToModuleName rels) locations
        where findLocation dn = fp <> resolveRel dn <> "/" <> (unpack . _moduleName . dottedToModuleName $ dn)
              resolveRel dn = replicate (countLeadingDots dn) '.'
              countLeadingDots = length
                               . takeWhile (== mempty)
                               . map ident_string
