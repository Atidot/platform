{-# LANGUAGE PackageImports #-}
module Main where

import "hspec"            Test.Hspec hiding (example)
import "platform-process" Platform.Process

main = hspec $ do
    return ()
