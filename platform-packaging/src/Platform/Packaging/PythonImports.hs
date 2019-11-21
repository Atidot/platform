{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Platform.Packaging.Extract where

import "base" GHC.Generics (Generic)
import "base" Data.Typeable (Typeable)
import "base" Data.Data (Data)

data PyPkg = 
    PyPkg
        { _pyPkg_index :: !URL
        , _pyPkg_name :: !Text

data Module = Module ()

instance Ord (Module, PypiPkg) where
    cmp p1 p2 = undefined

extractPkgs :: FilePath -> IO [PypiPkg]
extract = undefined

extractModules :: FilePath -> IO [Module]

findMatches :: [Text] -> IO [PyPIPkg]
findMatches = undefined

pkgHasModule :: PypiPkg -> Module -> IO Bool
pkgHasModule = undefined
