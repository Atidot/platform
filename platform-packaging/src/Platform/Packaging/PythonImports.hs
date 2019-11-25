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

extractPkgs :: FilePath -> IO [PyPkg]
extractPkgs = undefined

extractModules :: FilePath -> IO [Text]
extractModules = undefined

onlyImports = filter 

findMatches :: [Text] -> IO [PyPkg]
findMatches = undefined

pkgHasModule :: PyPkg 
             -> Module annot
             -> IO Bool
pkgHasModule = undefined

getImportNames :: Module annot -> [DottedName annot]
getImportNames (Module statements) = onlyJust $ map getImports' statements
  where getImports' i@Import{} = map (return . import_item_name) $ import_items i
        getImports' f@FromImport{} = map import_relative_module $ from_module f
        getImports' w@While{} = mapInto [while_body w, while_else w]
        getImports' f@For{} = mapInto [for_body f, for_else f]
        getImports' a@AsyncFor{} = mapInto [for_stmt a]
        getImports' f@Fun{} = mapInto [fun_body f]
        getImports' a@AsyncFun{} = mapInto [fun_def a]
        getImports' c@Class{} = mapInto [class_body c]
        getImports' c@Conditional{} = mapInto [map snd $ cond_guards c, cond_else c]
        getImports' d@Decorated{} = getImports' $ decorated_def d
        getImports' t@Try{} = mapInto [try_body t, try_else t, try_finally t]
        getImports' w@With{} = mapInto [with_body w]
        getImports' a@AsyncWith{} = getImports' $ with_stmt a
        getImports' _ = []
        mapInto = concatMap getImports'
        onlyJust (Nothing : xs) = xs
        onlyJust (Just x : xs) = x : onlyJust xs
        onlyJust [] = []

-- for TopLevel.MidLevel.ModName, this would guess
--   "TopLevel" > "TopLevel MidLevel" > "TopLevel MidLevel ModName"
-- > "MidLevel" > "MidLevel ModName" > "ModName"
-- Perhaps there are better guessing orders, but this one seems pretty reasonable.
pkgGuesses :: DottedName annot -> [Text]
pkgGuesses = map (pack . unwords) . supLevelSets . map ident_string
    where supLevelSets (x:xs) = (x:xs) : supLevelSets xs
          supLevelSets [] = []

isImport :: Statement annot -> Bool
isImport Import{} = True
isImport FromImport{} = True
isImport _ = False
