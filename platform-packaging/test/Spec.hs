{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
module Main where

import "base"               Control.Exception (evaluate)
import "hspec"              Test.Hspec
import "QuickCheck"         Test.QuickCheck
import "platform-packaging" Platform.Packaging.PythonImports.Internal

main :: IO ()
main = hspec platformPackagingPythonImports

platformPackagingPythonImports :: Spec
platformPackagingPythonImports = do
    describe "Platform.Packaging.getExplicitPkgOrigins" $ do
        it "returns Just [] given \"\"" $ do
            getExplicitPkgOrigins "" `shouldBe` (Just [])
        it "returns Just [] when the annotation does not follow an import" $ do
            getExplicitPkgOrigins "x = 2 #!platform annotation" `shouldBe` (Just [])
        it "returns Just [] when an import is not followed by a well-formed annotation" $ do
            getExplicitPkgOrigins "import tensorflow" `shouldBe` (Just [])
            getExplicitPkgOrigins "from tensorflow import keras" `shouldBe` (Just [])
            getExplicitPkgOrigins "import tensorflow #!latform tensorflow" `shouldBe` (Just [])
            getExplicitPkgOrigins "import tensorflow #platform tensorflow" `shouldBe` (Just [])
            getExplicitPkgOrigins "import tensorflow #!tensorflow" `shouldBe` (Just [])
        it "returns a pair given a well-formed annotation" $ do
            getExplicitPkgOrigins "import keras #!platform Keras" `shouldBe` (Just [kerasPair])
    where
        kerasPair = (ModuleName "keras", PyPkg "https://pypi.org/simple/" "Keras")
