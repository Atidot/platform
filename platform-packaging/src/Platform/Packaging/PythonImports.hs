{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Platform.Packaging.PythonImports
    ( ModuleName
    , PyPkg(..)
    , PythonImportException
    , runPythonImports
    , getAST
    ) where

import "base" Control.Monad.IO.Class (MonadIO, liftIO)
import "base" Control.Monad (when, unless, filterM, zipWithM, sequence_)
import "base" Data.Typeable (Typeable)
import "base" Data.Data (Data)
import "base" Data.List (foldl', intercalate)
import "base" GHC.Generics (Generic)
import "aeson" Data.Aeson (FromJSON, ToJSON, toEncoding, genericToEncoding, defaultOptions)
import "data-default" Data.Default (def)
import "text" Data.Text (Text, pack, unpack)
import qualified "text" Data.Text as T
import qualified "text" Data.Text.IO as TIO
import "extra"      Data.Tuple.Extra ((&&&))
import "extra" Control.Monad.Extra (concatMapM)
import "lens" Control.Lens
import "exceptions" Control.Monad.Catch (Exception, MonadMask, MonadCatch, MonadThrow, throwM, catchAll, catchIOError, bracket)
import "temporary" System.IO.Temp (withSystemTempDirectory)
import "regex-pcre" Text.Regex.PCRE
import "directory" System.Directory (getDirectoryContents, doesDirectoryExist, doesFileExist, setCurrentDirectory, getCurrentDirectory, removeFile, removeDirectoryRecursive, canonicalizePath)
import "language-python" Language.Python.Common.AST
import "language-python" Language.Python.Common.Token (Token, token_literal, token_span)
import "language-python" Language.Python.Common.SrcLocation (SrcSpan(SpanCoLinear))
import "language-python" Language.Python.Version3.Parser (parseModule, parseStmt)
import "language-python" Language.Python.Common.ParseError (ParseError)
import "shellmet" Shellmet
import Platform.Packaging.Pip
import Platform.Packaging.Pip.Types

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
    | NoSuchFile
    | MultipleSuchFiles
    deriving (Read, Eq, Ord, Bounded, Enum, Data, Typeable, Generic)

instance Show PythonImportException where
    show ModuleInNoPackages = "No package was found exporting the appropriate module name."
    show FileNotParseable = "This file could not be parsed as Python 3 code."

instance Exception PythonImportException

makeLenses ''InstallOpts
makeLenses ''DownloadOpts
makeLenses ''PipInput

data PkgVerdict
    = PkgContainsModule
    | PkgLacksModule
    | Inconclusive
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable, Data, Generic)

searchAndListNames :: (MonadCatch m, MonadIO m)
                   => Text
                   -> m [Text]
searchAndListNames pkg = do
    let firstWordRegex = "^[^ ]+(?= )" :: String
    t <- catchAll (liftIO . search def def $ pkg) (const $ return "")
    let matches = getAllTextMatches (unpack t =~ firstWordRegex :: AllTextMatches [] String)
    return $ map pack matches

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
    pkgs <- searchAndListNames $ _moduleName mn
    return $ map pypiPkg pkgs

findMatch :: (MonadMask m, MonadIO m)
          => ModuleName
          -> [PyPkg]
          -> m (Maybe PyPkg)
findMatch mn pkgs = do
    candidates <- filterM (fmap (== PkgContainsModule) . isModuleInPkg mn) pkgs
    if null candidates
       then return Nothing
       else return $ Just (head candidates)

-- Non-recursive top-level search.
-- The Maybe is here to fit the language-python API
foreignImportedModules :: Statement annot -> [Maybe (DottedName annot)]
foreignImportedModules i@Import{}     = map (return . import_item_name) . import_items $ i
foreignImportedModules i@FromImport{} = if 0 == (import_relative_dots $ from_module i)
                                          then return . import_relative_module . from_module $ i
                                          else []
foreignImportedModules _              = []

-- Non-recursive top-level search.
-- The Maybe is here to fit the language-python API
relativeImportedModules :: Statement annot -> [Maybe (DottedName annot)]
-- should the first pattern-match return []? TODO
relativeImportedModules i@Import{}     = map (return . import_item_name) . import_items $ i
relativeImportedModules i@FromImport{} = if 0 == (import_relative_dots $ from_module i)
                                            then []
                                            else return . import_relative_module . from_module $ i
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
    importNames <- map dottedToModuleName . getForeignImportNames <$> getAST fileContents
    explicitMatches <- mapM (getExplicitPkgOrigins . unpack . _moduleName) importNames
    possibleMatches <- mapM findPossibleMatches importNames
    matches <- zipWithM findMatch importNames possibleMatches
    return $ sortPairs (zip importNames matches)
    where
        -- This sorts matched pairs into the first tuple entry and unmatched
        -- pairs into the second tuple entry.
        sortPairs :: [(ModuleName, Maybe PyPkg)] -> ([(ModuleName, PyPkg)], [ModuleName])
        sortPairs pairs = (\(_, ys, zs) -> (ys, zs)) $ sortPairs' (pairs, [], [])
        sortPairs' ((m, Nothing) : xs, ys, zs) = sortPairs' (xs, ys, m : zs)
        sortPairs' ((m, Just x) : xs, ys, zs) = sortPairs' (xs, (m, x) : ys, zs)
        sortPairs' ([], ys, zs) = ([], ys, zs)

-- TODO: remove
makeVenv :: (MonadThrow m, MonadIO m)
         => FilePath
         ->  m ()
makeVenv fp = liftIO $ do
    "virtualenv" $| [pack fp]
    liftIO $ putStrLn $ fp <> "/bin/activate"
    "." $| [pack fp <> "/bin/activate"]
    return ()

-- TODO: remove
isModuleInPkg :: (MonadMask m, MonadIO m)
              => ModuleName
              -> PyPkg
              -> m PkgVerdict
isModuleInPkg modName pkg = do
        liftIO $ bracket init'
                         fini
                         body
    where
        init' = do
            --td <- withSystemTempDirectory "venv" return
            td <- return "/home/curran/testVenv"
            "mkdir" $| [pack td] -- TODO: set up -p option and mirror in the rm command somehow
            makeVenv td
            return td
        fini td = do
            "deactivate" $| []
            "rm" $| ["-rf", pack td]
            return ()
        body = \_ -> do
            isInstallable <- tryInstallPkg pkg
            if isInstallable
                then canImport modName
                else return Inconclusive

        canImport :: (MonadMask m, MonadIO m) => ModuleName -> m PkgVerdict
        canImport modName
          = liftIO $ catchIOError (("python" $| ["-c", "import", _moduleName modName]) >> return PkgContainsModule)
                              (const $ return PkgLacksModule)

tryInstallPkg :: (MonadCatch m, MonadIO m)
              => PyPkg
              -> m Bool
tryInstallPkg pkg = liftIO $ catchIOError (install def instOpts instInput >> return True)
                                          (const $ return False)
    where
        instOpts  = set installOpts_index (Just $ _pyPkg_index pkg) def
        instInput = ReqSpecInput [ReqSpec (_pyPkg_name pkg) Nothing]

downloadPkg :: (MonadCatch m, MonadIO m)
            => PyPkg
            -> FilePath
            -> m ()
downloadPkg pkg dir = do
    liftIO $ download def dlOpts dlInput
    return ()
    where
        dlOpts = set downloadOpts_index  (Just $ _pyPkg_index pkg)
               . set downloadOpts_noDeps (Just True)
               . set downloadOpts_build  (Just $ pack dir)
               $ def
        dlInput = ReqSpecInput [ReqSpec (_pyPkg_name pkg) Nothing]

getExplicitPkgOrigins :: (MonadThrow m, MonadIO m)
                      => String
                      -> m [([DottedName SrcSpan], PyPkg)]
getExplicitPkgOrigins fileContents = do
    comments <- getComments fileContents
    let annotationFiltered = filter hasPkgAnnotation comments
    explicitPkgs <- filterM comesAfterImport annotationFiltered
    mapM processPkgs explicitPkgs
    where
        processPkgs :: (MonadThrow m, MonadIO m) => Token -> m ([DottedName SrcSpan], PyPkg)
        processPkgs tkn = do
            stmt <- stmtBeforeComment tkn
            case stmt of
              Nothing   -> throwM FileNotParseable
              Just stmt -> return (getForeignImportNames $ Module [stmt], pkgFromToken tkn)
        comesAfterImport :: (MonadThrow m, MonadIO m) => Token -> m Bool
        comesAfterImport tkn = do
            stmt <- stmtBeforeComment tkn
            return . isImport $ stmt
        hasPkgAnnotation :: Token -> Bool
        hasPkgAnnotation tkn
          = ((token_literal tkn) =~ explicitPkgRegex
          || (token_literal tkn) =~ suppressPkgRegex)
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

pkgFromToken :: Token -> PyPkg
pkgFromToken tkn = PyPkg "pypi.org/simple" $ pack (token_literal tkn =~ explicitPkgRegex)

-- introspectWheel gives the modules exported by a package.
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
            contents <- liftIO $ getDirectoryContents fp
            return (dir, contents)
        fini (dir, originalContents) = do
            modifiedContents <- liftIO $ getDirectoryContents fp
            let newElements = filter (`notElem` originalContents) modifiedContents
            sequence_ $ map removeEntity newElements
            liftIO $ setCurrentDirectory dir
        removeEntity entity = do
            isDir <- liftIO $ doesDirectoryExist entity
            isFile <- liftIO $ doesFileExist entity
            unless (isDir || isFile) (throwM ImpossibleFileState)
            when   (isDir && isFile) (throwM ImpossibleFileState)
            when isDir (liftIO $ removeDirectoryRecursive entity)
            when isFile (liftIO $ removeFile entity)
        body _ = do
            liftIO $ setCurrentDirectory fp
            action fp

wheelAction :: (MonadThrow m, MonadIO m)
            => FilePath
            -> m [ModuleName]
wheelAction fp = do
    originalContents <- liftIO $ getDirectoryContents fp
    let wheelFiles = filter (\s -> s =~ ("\\.whl$" :: String)) originalContents
    wheelFile <- headIfLengthIsOne wheelFiles
    liftIO $ "unzip" $| [pack wheelFile]
    newContents <- liftIO
                  . fmap (filter (`notElem` originalContents))
                  . getDirectoryContents
                  $ fp
    subdirs <- liftIO $ filterM doesDirectoryExist newContents
    let distDirs = filter (=~ ("\\.dist-info$" :: String)) subdirs
    distDir <- headIfLengthIsOne distDirs
    -- the next line might fail if the .whl file is poorly formed.
    topLevelModules <- liftIO
                     . fmap (map (\name -> ModuleName name))
                     . fmap T.lines
                     . TIO.readFile
                     $ distDir <> "/top_level.txt"
    fmap concat . sequence
                . map (enumerateSubmodules fp)
                $ topLevelModules

tarAction :: (MonadThrow m, MonadIO m)
          => FilePath
          -> m [ModuleName]
tarAction fp = do
    originalContents <- liftIO $ getDirectoryContents fp
    let tarFiles = filter (\s -> s =~ ("\\.tar.gz$" :: String)) originalContents
    tarFile <- headIfLengthIsOne tarFiles
    liftIO $ "tar" $| ["-xzf", pack tarFile]
    newContents <- liftIO
                 . fmap (filter (`notElem` originalContents))
                 . getDirectoryContents
                 $ fp
    subdirs <- liftIO $ filterM doesDirectoryExist newContents
    eggInfoDirs <- liftIO
                 . fmap concat
                 . sequence
                 . map (\dir -> fmap (lines . unpack) $ "find" $| [pack dir, "-name", "*egg-info"])
                 $ subdirs -- [FilePath]
                 --[FilePath] -> [IO Text] -> [IO [String]] -> IO [[String]] -> IO [String] -> m [String]
    eggInfoDir <- headIfLengthIsOne eggInfoDirs
    dirAboveEggInfoDir <- liftIO
                        . canonicalizePath
                        $ eggInfoDir <> "/.."
    topLevelModules <- liftIO
                     . fmap (map (\name -> ModuleName name))
                     . fmap T.lines
                     . TIO.readFile
                     $ eggInfoDir <> "/top_level.txt"
    fmap concat . sequence
                . map (enumerateSubmodules dirAboveEggInfoDir)
                $ topLevelModules

enumerateSubmodules :: (MonadThrow m, MonadIO m)
                    => FilePath
                    -> ModuleName
                    -> m [ModuleName]
enumerateSubmodules fp mn = do
    initPresent <- liftIO . doesFileExist $ fp <> "/__init__.py"
    unless initPresent (throwM NoSuchFile)
    imports <- readInit $ fp <> "/__init__.py"
    return []
    --_ (<>) (return $ mods imports) (enumerateSubmodules (submods imports))
    --    where mods = map fst
    --          submods = undefined

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
