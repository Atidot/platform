{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
module Platform.Packaging.PythonImports.Internal.Utils where

import           "base"                     Control.Monad.IO.Class (MonadIO, liftIO)
import           "base"                     Control.Monad (join, when, unless, filterM, zipWithM, sequence_)
import           "base"                     Data.List (foldl', intercalate, isSuffixOf, partition, sortBy, (\\))
import           "base"                     Data.Maybe (isJust, fromJust)
import           "base"                     GHC.Generics (Generic)
import           "data-default"             Data.Default (def)
import qualified "fuzzyset"                 Data.FuzzySet as FS (fromList, get)
import           "text"                     Data.Text (Text, pack, unpack, replace, split)
import qualified "text"                     Data.Text as T
import qualified "text"                     Data.Text.IO as TIO
import           "extra"                    Data.Tuple.Extra ((&&&))
import           "extra"                    Control.Monad.Extra (concatMapM)
import           "lens"                     Control.Lens
import           "exceptions"               Control.Monad.Catch (MonadMask, MonadCatch, MonadThrow, throwM, catchAll, handleAll, catchIOError, bracket, catchIf)
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
import                                      Platform.Packaging.Pip

pypiPkg :: Text -> PyPkg
pypiPkg = PyPkg "https://pypi.org/simple/"

headIfLengthIsOne :: (MonadThrow m, MonadIO m)
                  => [a]
                  -> m a
headIfLengthIsOne xs = do
    when (length xs == 0) (throwM NoSuchFile)
    when (length xs >= 2) (throwM MultipleSuchFiles)
    return $ head xs

doParse :: String
        -> Maybe (Module SrcSpan, [Token])
doParse contents = do
    let parsed = parseModule contents "" -- We don't use last arg so leave it empty
    return' parsed
    where return' (Right p) = return p
          return' (Left _) = Nothing

getAST :: String
       -> Maybe (Module SrcSpan)
getAST = fmap fst . doParse

getComments :: String
            -> Maybe [Token]
getComments = fmap snd . doParse

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

getPythonFileNameRegex :: String
getPythonFileNameRegex = "[^\\/]+(?=.py$)"

containsInit :: (MonadThrow m, MonadIO m)
            => FilePath
            -> m Bool
containsInit fp = liftIO . doesFileExist $ fp <> "/__init__.py"

recursivelyConcatenate :: (MonadThrow m, MonadIO m)
                       => FilePath
                       -> m Text
recursivelyConcatenate fp = do
    isModule <- containsInit fp
    dirContents <- liftIO $ listDirectory fp
    if not isModule
       then return ""
       else do
           pyFiles <- fmap (filter (isSuffixOf ".py"))
                    . filterM (\location -> liftIO $ doesFileExist location)
                    $ dirContents
           localModules <- liftIO
                         . fmap T.concat
                         . sequence
                         $ map TIO.readFile pyFiles
           deeperModules <- fmap T.concat
                          . join
                          . fmap sequence
                          . fmap (map recursivelyConcatenate)
                          . liftIO
                          . filterM doesDirectoryExist
                          $ dirContents
           return $ localModules <> deeperModules

enumerateSubmodules :: (MonadCatch m, MonadIO m)
                    => FilePath -- The common prefix to be deleted
                    -> FilePath -- The location of the module
                    -> m [ModuleName]
enumerateSubmodules root fp = do
    initPresent <- containsInit fp
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
