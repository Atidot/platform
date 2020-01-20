{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module PkgDep where

import "base"                     Data.Maybe (maybe)
import "base"                     Data.Typeable (Typeable)
import "base"                     GHC.Generics (Generic)
import "base"                     System.IO
import "exceptions"               Control.Monad.Catch (Exception, throwM)
import "aeson"                    Data.Aeson (decode)
import "bytestring"               Data.ByteString.Lazy.Char8 as B8 (pack)
import "data-default"             Data.Default
import "dockerfile"               Data.Docker (Docker, dockerfile)
import "text"                     Data.Text as T (Text, pack, unpack)
import "directory"                System.Directory (doesDirectoryExist, doesFileExist)
import "turtle"                   Turtle (Shell, Line, inproc, textToLine, sh)
import "optparse-generic"         Options.Generic (getRecord, ParseRecord)
import "platform-packaging"       Platform.Packaging.PythonImports
import "platform-packaging-types" Platform.Packaging.Types (ContainerEnv)
import                            Atidot.Platform.Deployment
import                            Atidot.Platform.Deployment.Interpreter.Terraform
import                            Atidot.Platform.Deployment.Interpreter.AMI.Types

data CLI
  = Execute
  { input :: String
  , env :: Maybe String
  , containerLocation :: Maybe String
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
    let loc = maybe "./container.tar.gz" id (containerLocation record)
    isDir <- doesDirectoryExist fp
    isFile <- doesFileExist fp
    if (isDir && isFile) || (not isDir && not isFile)
       then error "Impossible file state"
       else return ()
    docker <- if isDir
                 then do
                     modules <- fmap T.unpack $ recursivelyConcatenate fp
                     pyToDocker modules env' Nothing
                 else do
                     handle <- openFile fp ReadMode
                     contents <- hGetContents handle
                     pyToDocker contents
    sh $ buildDocker docker loc
    runTerraform def $ do
        s <- secret placeHolderSecret
        dir <- mount placeHolderData
        c <- container . T.pack $ loc
        attachSecret s c
        attachVolume dir c
    where pyToDocker = undefined

buildDocker :: Docker ()
            -> FilePath
            -> Shell Line
buildDocker d output = do
    let dockerfileAsText = T.pack . dockerfile $ d
    -- The following args let us build a dockerfile from stdin
    let maybeLine = fmap return $ textToLine dockerfileAsText
    maybe (throwM $ DockerfileEmpty dockerfileAsText)
          (inproc "docker" ["build", "-q", "-o", T.pack output, "-", "<"])
          maybeLine
