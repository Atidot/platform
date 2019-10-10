{-# LANGUAGE PackageImports #-}
module Main where

import "hspec"               Test.Hspec hiding (example)
import "platform-types" Platform.Types


main :: IO ()
main = hspec $ do
    return ()
