{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module PkgDep where

import "base"                     Data.Maybe (maybe)
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
import                            Atidot.Platform.Deployment.Interpreter.AMI.Types

data CLI
  = Execute
  { input :: String
  , env :: Maybe String
  } deriving (Generic, Show)

instance ParseRecord CLI where

data PkgDepException
  = DockerfileEmpty T.Text
  deriving (Show, Typeable)

instance Exception PkgDepException

main :: IO ()
main = getRecord "Packaging Deployment" >>= \record -> do
    let fp = input record
    let env' = fmap B8.pack (env record) >>= decode :: Maybe ContainerEnv
    isDir <- doesDirectoryExist fp
    modules <- if isDir
                  then do
                      fmap T.unpack $ recursivelyConcatenate fp
                  else do
                      handle <- openFile fp ReadMode
                      hGetContents handle
    docker <- pythonToContainerEnv modules $ maybe testingEnv id env'
    rootDir <- dockerRootDir
    runTerraform def $ do
        imageName <- makeContainer docker
        s <- secret placeHolderSecret
        dir <- mount placeHolderData
        c <- container $ rootDir <> "/containers/" <> imageName
        attachSecret s c
        attachVolume dir c

dockerRootDir :: IO Text
dockerRootDir = do
    info <- fmap T.unpack . strict
          $ inproc "docker" ["info"]
          $ select . textToLines $ ""
    let matchedDir = info =~ ("(?<=^Docker Root Dir: ).+$" :: String) :: AllTextMatches [] String
    return . T.pack . head . getAllTextMatches $ matchedDir
