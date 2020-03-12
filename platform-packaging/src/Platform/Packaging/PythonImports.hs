{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE TemplateHaskell #-}
module Platform.Packaging.PythonImports
    ( ModuleName
    , PyPkg(..)
    , PythonImportException
    , runPythonImports
    , recursivelyConcatenate
    , getAST
    , containsInit
    ) where

import           "base"                     Debug.Trace (trace)
import           "base"                     Control.Monad (zipWithM)
import           "base"                     Control.Monad.IO.Class (MonadIO)
import           "base"                     Data.Semigroup ((<>))
import           "base"                     Data.List (partition, (\\), find)
import           "base"                     Data.Maybe (isJust, fromJust, fromMaybe, maybe)
import           "lens"                     Control.Lens
import           "exceptions"               Control.Monad.Catch (MonadMask, throwM, handleAll)
import           "mtl"                      Control.Monad.State.Lazy
import           "data-default"             Data.Default (Default, def)
import           "containers"               Data.Map.Strict (Map, keys)
import qualified "containers"               Data.Map.Strict as M
import qualified "containers"               Data.Set as S (fromList, notMember)
import qualified "text"                     Data.Text as T (pack)
import           "platform-packaging-types" Platform.Packaging.PythonImports.Types
import Platform.Packaging.Pip
import Platform.Packaging.PythonImports.Internal.Annotation
import Platform.Packaging.PythonImports.Internal.PipMatches
import Platform.Packaging.PythonImports.Internal.PipMatches.ParseStdLibHTML
import Platform.Packaging.PythonImports.Internal.Utils

data PythonImportsState
    = PythonImportsState
    { _foundModules :: Map PyPkg [ModuleName]
    , _successfulMatches :: [(ModuleName, PyPkg)]
    , _failedMatches :: [ModuleName]
    } deriving (Show, Eq)

instance Default PythonImportsState where
    def = PythonImportsState mempty mempty mempty

makeLenses ''PythonImportsState

-- The first element of the output tuple contains successful pairings.
-- The second element contains modules which were not matched with a package.
runPythonImports :: (MonadMask m, MonadIO m)
                 => String
                 -> m ([(ModuleName, PyPkg)], [ModuleName])
runPythonImports fileContents = do
    -- The following line is silent if scraper parsing fails--TODO: change
    stdLibModsRaw <- fromMaybe [] <$> liftIO scrapeModules
    let stdLibMods = S.fromList $ map (ModuleName . T.pack) stdLibModsRaw
    unmatchedMods <- filter (\mn -> mn `S.notMember` stdLibMods)
                   . map dottedToModuleName
                   . getForeignImportNames
                 <$> maybe (throwM FileNotParseable) return (getAST fileContents)
    let explicitMatches = fromMaybe [] $ getExplicitPkgOrigins fileContents
        unmatchedMods'  = unmatchedMods \\ map fst explicitMatches
    possibleMatches <- mapM findPossibleMatches unmatchedMods'
    let possibleMatchesTruncated = map (take 10) possibleMatches --TODO: calibrate this
    evalStateT (run $ zip unmatchedMods' possibleMatchesTruncated) def

-- Keeps track of _foundModules to reduce the number of calls to `pip download` etc.
-- The _successfulMatches and _failedMatches become the output values.
run :: (MonadMask m, MonadIO m)
    => [(ModuleName, [PyPkg])]
    -> StateT PythonImportsState m ([(ModuleName, PyPkg)], [ModuleName])
run [] = (,) <$> fmap _successfulMatches get <*> fmap _failedMatches get
run ((mn, possibleMatches) : remainder) = do
    s <- get
    trace (show s) $ return ()
    let previousMatch = find (\pypkg -> fromMaybe False
                                     . fmap (elem mn)
                                     . M.lookup pypkg
                                     $ _foundModules s)
                             possibleMatches
    trace ("Getting matches for " ++ show mn) $ maybe searchForMatch recordPreviousMatch previousMatch
    where
        recordPreviousMatch pkg = do
            put . (successfulMatches <>~ [(mn, pkg)]) =<< get
            run remainder
        searchForMatch = do
            s <- get
            let uninspectedPossibilities = possibleMatches \\ (keys . _foundModules $ s)
            (packageExports, packageWhichExportsMn) <- efficientlyMatch mn uninspectedPossibilities
            put . (foundModules <>~ packageExports)
                . maybe (failedMatches <>~ [mn])
                        (\pypkg -> successfulMatches <>~ [(mn, pypkg)])
                        packageWhichExportsMn
                $ s
            run remainder

-- Steps through a list of PyPkg's, recording the modules belonging to it.
-- When the ModuleName passed to this function is found to belong to a PyPkg,
-- the traversal terminates.
efficientlyMatch :: (MonadMask m, MonadIO m)
                 => ModuleName
                 -> [PyPkg]
                 -> m (Map PyPkg [ModuleName], Maybe PyPkg)
efficientlyMatch mn []         = return (mempty, Nothing)
-- This `handleAll` call means an error in `modulesInPkg pkg` is interpreted as though `pkg` exports nothing.
efficientlyMatch mn (pkg:pkgs) = handleAll (const $ nextStep mempty) $ do
    pkgExports <- modulesInPkg pkg
    let matching = M.fromList [(pkg, pkgExports)]
    if mn `elem` pkgExports
       then return (matching, Just pkg)
       else nextStep matching
    where
        nextStep m = do
            (matching', pkg') <- efficientlyMatch mn pkgs
            return (matching' <> m, pkg') -- Data.Map.(<>) prioritizes the left argument
