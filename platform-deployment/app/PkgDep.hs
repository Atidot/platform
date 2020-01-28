{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module PkgDep where

import "base"                     Data.Maybe (fromMaybe)
import "base"                     Data.Typeable (Typeable)
import "base"                     Data.Monoid (mempty)
import "base"                     GHC.Generics (Generic)
import "base"                     System.IO
import "exceptions"               Control.Monad.Catch (Exception, throwM)
import "aeson"                    Data.Aeson (decode)
import "bytestring"               Data.ByteString.Lazy.Char8 as B8 (pack)
import "data-default"             Data.Default
import "dockerfile"               Data.Docker (Docker, dockerfile)
import "text"                     Data.Text as T (Text, pack, unpack)
import "regex-pcre"               Text.Regex.PCRE
import "directory"                System.Directory (doesDirectoryExist, doesFileExist)
import "turtle"                   Turtle (Shell, Line, inproc, inshell, textToLines, strict, view, select)
import "optparse-generic"         Options.Generic (getRecord, ParseRecord)
import "platform-packaging"       Platform.Packaging (pythonToContainerEnv, testingEnv)
import "platform-packaging"       Platform.Packaging.PythonImports
import "platform-packaging-types" Platform.Packaging.Types (ContainerEnv)
import                            Atidot.Platform.Deployment
import                            Atidot.Platform.Deployment.Interpreter.Terraform
import                            Atidot.Platform.Deployment.Interpreter.Terraform.Template
import                            Atidot.Platform.Deployment.Interpreter.AMI.Types

data CLI
  = Execute
  { dockerenv :: Maybe String
  , config :: Maybe String
  } deriving (Generic, Show)

instance ParseRecord CLI where

data PkgDepException
  = DockerfileEmpty T.Text
  deriving (Show, Typeable)

instance Exception PkgDepException

main :: IO ()
main = getRecord "Packaging Deployment" >>= \record -> do
    let env' = fromMaybe def $ fmap B8.pack (dockerenv record) >>= decode :: ContainerEnv
    let terraformConfig = fromMaybe def $ fmap B8.pack (config record) >>= decode :: TerraformExtendedConfig
    runTerraform terraformConfig $ do
        imageName <- makeContainer env' "/path/to/python/lib" -- docker
        s <- secret placeHolderSecret
        dir <- mount placeHolderData
        c <- container imageName -- rootDir <> "/containers/" <> imageName
        attachSecret s c
        attachVolume dir c
