{-# LANGUAGE DeriveGeneric     #-}
module PkgDep where

import "data-default"     Data.Default
import                    Atidot.Platform.Deployment
import                    Atidot.Platform.Deployment.Interpreter.Terraform
import                    Atidot.Platform.Deployment.Interpreter.AMI.Types

main :: IO ()
main = do
    print "your code here"
    runTerraform def $ do
        s <- secret placeHolderSecret
        dir <- mount placeHolderData
        c <- container "/path/to/docker.tar.gz" -- <-- modify the string
        attachSecret s c
        attachVolume dir c
