{-# LANGUAGE DeriveGeneric     #-}
module Main where

import "optparse-generic" Options.Generic
import "data-default"     Data.Default
import                    Atidot.Platform.Deployment
import                    Atidot.Platform.Deployment.Interpreter.Terraform
import                    Atidot.Platform.Deployment.Interpreter.AMI.Types

data CLI = CLI
    {script :: String
    } deriving (Generic, Show)

instance ParseRecord CLI


main :: IO ()
main = do
    x <- script <$> getRecord "Platform Deployment"
    case lookup x scripts of
        Just s -> runTerraform def s
        Nothing -> error $ unlines
            [ "script '" ++ x ++ "' not found"
            , "available scripts are: " ++ show (map fst scripts)
            ]

scripts =
    [ ("nsss",nsss)
    , ("kiss",kiss)
    , ("nes",noneExistentSecret)
    , ("nesa",noneExistentSecretAttached)
    , ("sdt", secretDeclaredTwice)
    , ("sat",secretAttachedTwice)
    , ("cdne",containerDoesNotExists)
    ]