{-# LANGUAGE PackageImports #-}
module Main where

import "hspec"            Test.Hspec hiding (example)
import "platform-types"   Platform.Types
import "platform-dsl"     Platform.DSL
import "platform-harness" Platform.Harness


main :: IO ()
main = hspec $ do
    return ()
