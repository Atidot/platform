{-# LANGUAGE PackageImports #-}
module Main where

import "data-default"       Data.Default (def)
import "hspec"              Test.Hspec hiding (example)
import                      Atidot.Platform.Deployment
import                      Atidot.Platform.Deployment.Interpreter.Terraform
import                      Spec.TestScripts

main :: IO ()
main = hspec $ do
    describe "Atidot.Platform.Deployment.Interpreter.Terraform.runTerraform" $ do
        mapM_ runTerraformTest errorThrowingScripts
        it "Passes the hello script without error." $ do
            runTerraform def hello `shouldReturn` ()
    where
        runTerraformTest (n, s, e) = it ("Throws the correct error in the script codenamed '" <> n <> "'.")
                                        (expectError s e)
        expectError s e = (runTerraform def s) `shouldThrow` errorCall e

errorThrowingScripts :: [(String, DeploymentM (), String)]
errorThrowingScripts =
    [ ("nes", noneExistentSecret, "secret 'some/secret' not found")
    , ("nesa", noneExistentSecretAttached, "secret 'some/secret' not found for attachment")
    , ("sdt", secretDeclaredTwice, "secret 'tutorials/MyFirstTutorialSecret' already exists")
    , ("sat", secretAttachedTwice, "secret 'tutorials/MyFirstTutorialSecret' is already attached to container 'hello-world'")
    , ("cdne", containerDoesNotExists, "container 'hello-world' does not exist") ]
