{-# LANGUAGE PackageImports #-}
module Platform.Packaging.PythonImports
    ( ModuleName
    , PyPkg(..)
    , PythonImportException
    , runPythonImports
    , recursivelyConcatenate
    , getAST
    , containsInit
    ) where

import "base"                     Control.Monad (zipWithM)
import "base"                     Control.Monad.IO.Class (MonadIO)
import "base"                     Data.List (partition, (\\))
import "base"                     Data.Maybe (isJust, fromJust)
import "exceptions"               Control.Monad.Catch (MonadMask, throwM)
import "platform-packaging-types" Platform.Packaging.PythonImports.Types
import Platform.Packaging.Pip
import Platform.Packaging.PythonImports.Internal.Annotation
import Platform.Packaging.PythonImports.Internal.PipMatches
import Platform.Packaging.PythonImports.Internal.Utils

runPythonImports :: (MonadMask m, MonadIO m)
                 => String
                 -> m ([(ModuleName, PyPkg)], [ModuleName])
runPythonImports fileContents = do
    unmatchedMods <- map dottedToModuleName
                   . getForeignImportNames
                 <$> maybe (throwM FileNotParseable) return (getAST fileContents)
    let explicitMatches = maybe [] id (getExplicitPkgOrigins fileContents)
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
