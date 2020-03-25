{-# LANGUAGE PackageImports   #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
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
import           "exceptions"               Control.Monad.Catch (MonadMask, throwM, handleIOError, handleAll)
import           "mtl"                      Control.Monad.State.Lazy
import           "mtl"                      Control.Monad.Writer.Lazy
import           "data-default"             Data.Default (Default, def)
import           "containers"               Data.Map.Strict (Map, keys)
import qualified "containers"               Data.Map.Strict as M
import qualified "containers"               Data.Set as S (map, fromList, notMember)
import qualified "text"                     Data.Text as T (pack)
import           "platform-packaging-types" Platform.Packaging.PythonImports.Types
import Platform.Packaging.Pip
import Platform.Packaging.PythonImports.Internal.Annotation
import Platform.Packaging.PythonImports.Internal.PipMatches
import Platform.Packaging.PythonImports.Internal.PipMatches.ParseStdLibHTML
import Platform.Packaging.PythonImports.Internal.Utils

data PythonImportsState
    = PythonImportsState
    { _inspectedModules :: Map PyPkg [ModuleName]
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
    foreignMods   <- map dottedToModuleName
                   . getForeignImportNames
                 <$> maybe (throwM FileNotParseable) return (getAST fileContents)
    let explicitMatches = S.fromList . fromMaybe [] $ getExplicitPkgOrigins fileContents
        unmatchedMods   = filter (\mn -> mn `S.notMember` stdLibMods
                                      && mn `S.notMember` S.map fst explicitMatches)
                                 foreignMods
    possibleMatches <- mapM findPossibleMatches unmatchedMods
    let possibleMatchesTruncated = map (take 10) possibleMatches --TODO: calibrate this
        tryToMatch' = uncurry tryToMatch
        stateProgram = mapM_ tryToMatch' $ zip unmatchedMods possibleMatchesTruncated
    outRaw <- execStateT stateProgram def
    return (_successfulMatches outRaw, _failedMatches outRaw)

-- Keeps track of _inspectedModules to reduce the number of calls to `pip download` etc.
-- The _successfulMatches and _failedMatches become the output values.
tryToMatch :: (MonadMask m, MonadIO m)
           => ModuleName
           -> [PyPkg]
           -> StateT PythonImportsState m ()
tryToMatch mn pkgs = do
    state <- get
    let previousMatch = find (previousMatchPred state) pkgs
    maybe (searchForMatch state) (recordPreviousMatch state) previousMatch
        where
            previousMatchPred s pypkg = fromMaybe False
                                      . fmap (elem mn)
                                      . M.lookup pypkg
                                      $ _inspectedModules s

            recordPreviousMatch s pkg
                = put . (successfulMatches <>~ [(mn, pkg)]) $ s

            uninspectedPossibilities s = pkgs \\ (keys . _inspectedModules $ s)

            searchForMatch s = do
                matchOutcome <- trace ("trying to match " <> show mn) $ runWriterT $ efficientlyMatch mn (uninspectedPossibilities s)
                let packageWhichExportsMn = fst matchOutcome
                    packageExports        = snd matchOutcome
                put . (inspectedModules <>~ packageExports)
                    . maybe (failedMatches <>~ [mn])
                            (\pypkg -> successfulMatches <>~ [(mn, pypkg)])
                            packageWhichExportsMn
                    $ s

-- Steps through a list of PyPkg's, recording the modules belonging to it.
-- When the ModuleName passed to this function is found to belong to a PyPkg,
-- the traversal terminates.
efficientlyMatch :: (MonadMask m, MonadIO m)
                 => ModuleName
                 -> [PyPkg]
                 -> WriterT (Map PyPkg [ModuleName]) m (Maybe PyPkg)
efficientlyMatch mn []         = return Nothing
efficientlyMatch mn (pkg:pkgs) = do
    thisOneMatched <- trace ("one iteration of isAMatch " <> show mn <> show pkg) $ isAMatch mn pkg
    if thisOneMatched
       then trace "MATCHED!" $ return $ Just pkg
       else efficientlyMatch mn pkgs

isAMatch :: (MonadMask m, MonadIO m)
          => ModuleName
          -> PyPkg
          -> WriterT (Map PyPkg [ModuleName]) m Bool
-- This 'handleAll' call means an error in 'isAMatch' is interpreted as though 'pkg' exports nothing.
isAMatch mn pkg = handleIOError (const $ return False) $ do
    pkgExports <- modulesInPkg pkg
    tell $ M.fromList [(pkg, pkgExports)]
    return $ mn `elem` pkgExports
