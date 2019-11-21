{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Platform.Packaging.PythonImports where

import "base" GHC.Generics (Generic)
import "base" Data.Typeable (Typeable)
import "base" Data.Data (Data)
import "aeson" Data.Aeson (FromJSON, ToJSON, toEncoding, genericToEncoding, defaultOptions)

type URL = Text

data PyPkg 
    = PyPkg
    { _pyPkg_index :: !URL
    , _pyPkg_name :: !Text
    } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance ToJSON PyPkg where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON PyPkg where

newtype Module
    = Module 
    { _module_names :: ![Text] 
    } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance Ord (Module, PypiPkg) where
    cmp p1 p2 = undefined

extractPkgs :: FilePath -> IO [PypiPkg]
extract = undefined

extractModules :: FilePath -> IO [Module]

findMatches :: [Text] -> IO [PyPIPkg]
findMatches = undefined

pkgHasModule :: PypiPkg -> Module -> IO Bool
pkgHasModule = undefined
