{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Platform.Packaging.PythonImports where

import "base" GHC.Generics (Generic)
import "base" Data.Typeable (Typeable)
import "base" Data.Data (Data)
import "base" Data.List (foldl')
import "aeson" Data.Aeson (FromJSON, ToJSON, toEncoding, genericToEncoding, defaultOptions)
import "text" Data.Text (Text, pack)
import "language-python" Language.Python.Common.AST 
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

newtype ModuleName = ModuleName { _moduleName :: !Text }
    deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance ToJSON ModuleName where 
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ModuleName where

extractPkgs :: FilePath -> IO [PyPkg]
extractPkgs = undefined

extractModules :: FilePath -> IO [ModuleName]
extractModules = undefined

findPossibleMatches :: ModuleName -> IO [PyPkg]
findPossibleMatches = undefined

findMatch :: ModuleName -> IO PyPkg
findMatch = undefined

pkgHasModule :: PyPkg 
             -> ModuleName
             -> IO Bool
pkgHasModule = undefined

getImportNames :: Module annot -> [DottedName annot]
getImportNames (Module statements) = onlyJust . concatMap getImports' $ statements
  where getImports' :: Statement annot -> [Maybe (DottedName annot)]
        getImports' i@Import{}      = map (return . import_item_name) . import_items $ i
        getImports' f@FromImport{}  = return . import_relative_module . from_module $ f
        getImports' w@While{}       = recurse . (while_body <> while_else) $ w
        getImports' f@For  {}       = recurse . (for_body <> for_else) $ f
        getImports' a@AsyncFor{}    = recurse . for_stmt $ a
        getImports' f@Fun{}         = recurse . fun_body $ f
        getImports' a@AsyncFun{}    = recurse . fun_def $ a
        getImports' c@Class{}       = recurse . class_body $ c
        getImports' c@Conditional{} = recurse . ((map snd . cond_guards) <> cond_else) $ c
        getImports' d@Decorated{}   = recurse . decorated_def $ d
        getImports' t@Try{}         = recurse . (try_body <> try_else <> try_finally) $ t
        getImports' w@With{}        = recurse . with_body $ w
        getImports' a@AsyncWith{}   = recurse . with_stmt $ a
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
