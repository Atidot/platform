{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Platform.Packaging.Extract where

data PypiPkg = PypiPkg ()

data Module = Module ()

instance Ord (Module, PypiPkg) where
    cmp p1 p2 = undefined

extractPkgs :: FilePath -> IO [PypiPkg]
extract = undefined

extractModules :: FilePath -> IO [Module]

findMatches :: [Text] -> IO [PypiPkg]
findMatches = undefined

pkgHasModule :: PypiPkg -> Module -> IO Bool
pkgHasModule = undefined
