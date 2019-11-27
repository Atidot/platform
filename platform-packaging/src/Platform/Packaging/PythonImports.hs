{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Platform.Packaging.PythonImports where

import "base" Control.Monad.IO.Class (MonadIO, liftIO)
import "base" Control.Monad (when, unless, filterM)
import "base" Data.Typeable (Typeable)
import "base" Data.Data (Data)
import "base" Data.List (foldl', intercalate)
import "base" GHC.Generics (Generic)
import "base" System.IO
import "aeson" Data.Aeson (FromJSON, ToJSON, toEncoding, genericToEncoding, defaultOptions)
import "data-default" Data.Default (def)
import "text" Data.Text (Text, pack)
import "extra"      Data.Tuple.Extra ((&&&))
import "exceptions" Control.Monad.Catch (Exception, MonadMask, MonadThrow, throwM, bracket)
import "regex-tdfa" Text.Regex.TDFA
import "regex-tdfa-text" Text.Regex.TDFA.Text ()
import "language-python" Language.Python.Common.AST 
import "language-python" Language.Python.Common.SrcLocation (SrcSpan)
import qualified "language-python" Language.Python.Version2.Parser as V2
import qualified "language-python" Language.Python.Version3.Parser as V3
import Platform.Packaging.Pip

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
    = RegexFailure
    | ModuleInNoPackages
    | FileNotParseable
    | NotAFilePath
    deriving (Read, Eq, Ord, Bounded, Enum, Data, Typeable, Generic)

instance Show PythonImportException where
    show RegexFailure = "There was a failure to match on a regex that was expected to have at least one match."
    show ModuleInNoPackages = "No package was found exporting the appropriate module name."
    show FileNotParseable = "This file was not parsed as either Python 2 or Python 3."
    show NotAFilePath = "A path to a file was expected, but this string is not one."

instance Exception PythonImportException

searchAndListNames :: (MonadMask m, MonadIO m)
                   => Text
                   -> m [Text]
searchAndListNames pkg = do
    let firstWordRegex = "^[^ ]+(?= )"
    t <- liftIO . search def def $ pkg
    return $ getAllTextMatches (t =~ firstWordRegex)

pypiPkg :: Text -> PyPkg
pypiPkg = PyPkg "https://pypi.org/simple/"

getAST :: (MonadThrow m, MonadMask m, MonadIO m) 
       => FilePath 
       -> m (Module SrcSpan)
getAST fp = do
    let afterLastSlashRegex = "(?<=/)[^/]+$" -- capture from the final slash to EOL
    let fileName = fp =~ afterLastSlashRegex
    when (fileName == "") $ throwM NotAFilePath
    handle <- liftIO $ openFile fp ReadMode
    contents <- liftIO $ hGetContents handle
    let parsed = V3.parseModule contents fileName
    let finalParsed = if isRight parsed 
                         then parsed 
                         else V2.parseModule contents fileName
    return' finalParsed
    where isRight (Right _) = True
          isRight _         = False 
          return' (Right p) = return $ fst p
          return' (Left _) = throwM FileNotParseable

findPossibleMatches :: (MonadMask m, MonadIO m)
                    => ModuleName 
                    -> m [PyPkg]
findPossibleMatches mn = do
    pkgs <- searchAndListNames $ _moduleName mn
    return $ map pypiPkg pkgs

findMatch :: (MonadMask m, MonadIO m, MonadThrow m)
          => ModuleName 
          -> [PyPkg]
          -> m PyPkg
findMatch mn pkgs = do
    candidates <- filterM (`pkgHasModule` mn) pkgs
    when (null candidates) $ throwM ModuleInNoPackages
    return $ head candidates

pkgHasModule :: (MonadMask m, MonadIO m)
             => PyPkg 
             -> ModuleName
             -> m Bool
pkgHasModule = undefined

getImportNames :: Module annot -> [DottedName annot]
getImportNames (Module statements) = onlyJust . concatMap getImports' $ statements
  where getImports' :: Statement annot -> [Maybe (DottedName annot)]
        getImports' i@Import{}      = map (return . import_item_name) . import_items $ i
        getImports' f@FromImport{}  = return . import_relative_module . from_module $ f
        getImports' w@While{}       = recurse . (while_body <> while_else) $ w
        getImports' f@For  {}       = recurse . (for_body <> for_else) $ f
        getImports' a@AsyncFor{}    = recurse . return . for_stmt $ a
        getImports' f@Fun{}         = recurse . fun_body $ f
        getImports' a@AsyncFun{}    = recurse . return . fun_def $ a
        getImports' c@Class{}       = recurse . class_body $ c
        getImports' c@Conditional{} = recurse . mconcat . ((map snd . cond_guards) <> (return . cond_else)) $ c
        getImports' d@Decorated{}   = recurse . return . decorated_def $ d
        getImports' t@Try{}         = recurse . (try_body <> try_else <> try_finally) $ t
        getImports' w@With{}        = recurse . with_body $ w
        getImports' a@AsyncWith{}   = recurse . return . with_stmt $ a
        getImports' _               = []

        recurse :: [Statement annot] -> [Maybe (DottedName annot)]
        recurse = concatMap getImports'

        onlyJust :: [Maybe a] -> [a]
        onlyJust (Nothing : xs) = onlyJust xs
        onlyJust (Just x : xs)  = x : onlyJust xs
        onlyJust []             = []

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
dottedToModuleName dn = ModuleName $ pack . intercalate "," . map ident_string $ dn

runPythonImports :: (Monad m, MonadMask m, MonadIO m)
                 => FilePath
                 -> m [PyPkg]
runPythonImports fp
    = bracket init'
              fini
              body
    where
        init'  = return ()
        fini _ = return ()
        
        body _ = do
            importNames <- map dottedToModuleName . getImportNames <$> getAST fp
            possibleMatches <- map findPossibleMatches importNames
            --let matches' = zip importNames possibleMatches
            --let matchActions = map (uncurry findMatch) matches'
            return . map head $ possibleMatches

            -- fp :: FilePath (getAST ->) m (Module annot) (getImportNames <$> ->)
            -- m [DottedName annot] -> m [ModuleName] (given to) importNames
            -- [ModuleName] -> [m [PyPkg]]
