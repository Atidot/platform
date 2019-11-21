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

type ModuleNames = [Text]

extractPkgs :: FilePath -> IO [PypiPkg]
extract = undefined

extractModules :: FilePath -> IO [Module]
extractModules = undefined

onlyImports = filter 

findMatches :: [Text] -> IO [PyPIPkg]
findMatches = undefined

pkgHasModule :: PypiPkg -> Module -> IO Bool
pkgHasModule = undefined

getImports :: Module -> [Statement]
getImports (Module statements) = map getImports' statements
  where getImports' i@Imports{} = i
        getImports' f@FromImports{} = f
        getImports' w@While{} = mapInto [while_body w, while_else w]
        getImports' f@For{} = mapInto [for_body f, for_else f]
        getImports' a@AsyncFor{} = mapInto [for_stmt a]
        getImports' f@Fun{} = mapInto [fun_body f]
        getImports' a@AsyncFun{} = mapInto [fun_def a]
        getImports' c@Class{} = mapInto [class_body c]
        getImports' c@Conditional{} = mapInto [map snd $ cond_guards c, cond_else c]
        getImports' d@Decorated{} = getImports $ decorated_def d
        getImports' t@Try{} = mapInto [try_body t, try_else t, try_finally t]
        getImports' w@With{} = mapInto [with_body w]
        getImports' a@AsyncWith{} = getImports $ with_stmt a
        getImports' _ = []
        mapInto ss = mconcat (map getImports ss)

isImport :: Statement -> Bool
isImport Import{} = True
isImport FromImport{} = True
isImport _ = False
