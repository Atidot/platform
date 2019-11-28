{-# LANGUAGE DeriveGeneric #-}
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
import "aeson" Data.Aeson (FromJSON, ToJSON, toEncoding, genericToEncoding, defaultOptions)
import "data-default" Data.Default (def)
import "text" Data.Text (Text, pack, unpack)
import "extra"      Data.Tuple.Extra ((&&&))
import "exceptions" Control.Monad.Catch (Exception, MonadMask, MonadThrow, throwM, bracket)
import "regex-pcre" Text.Regex.PCRE
import "language-python" Language.Python.Common.AST 
import "language-python" Language.Python.Common.SrcLocation (SrcSpan)
import "language-python" Language.Python.Version3.Parser (parseModule)
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
    show FileNotParseable = "This file could not be parsed as Python 3 code."
    show NotAFilePath = "A path to a file was expected, but this string is not one."

instance Exception PythonImportException

searchAndListNames :: (MonadMask m, MonadIO m)
                   => Text
                   -> m [Text]
searchAndListNames pkg = do
    let firstWordRegex = "^[^ ]+(?= )" :: String
    t <- liftIO . search def def $ pkg
    let matches = getAllTextMatches (unpack t =~ firstWordRegex :: AllTextMatches [] String)
    return $ map pack matches

pypiPkg :: Text -> PyPkg
pypiPkg = PyPkg "https://pypi.org/simple/"

getAST :: (MonadThrow m) 
       => String 
       -> m (Module SrcSpan)
getAST contents = do
    let parsed = parseModule contents "" -- We don't use last arg so leave it empty
    return' parsed
    where return' (Right p) = return $ fst p
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

-- TODO: Write out the proper package-inspection logic to check this.
pkgHasModule :: (MonadMask m, MonadIO m)
             => PyPkg 
             -> ModuleName
             -> m Bool
pkgHasModule _ _ = return True

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
                 => String
                 -> m [PyPkg]
runPythonImports fileContents = do
    importNames <- map dottedToModuleName . getImportNames <$> getAST fileContents
    possibleMatches <- mapM findPossibleMatches importNames
    --let matches' = zip importNames possibleMatches
    --let matchActions = map (uncurry findMatch) matches'
    return $ map head possibleMatches
