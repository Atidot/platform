{-# LANGUAGE PackageImports #-}
module Main where

import           "base"                    System.IO (stdin)
import           "directory"               System.Directory (getCurrentDirectory)
import qualified "bytestring"              Data.ByteString.Lazy.Char8 as B8 (putStrLn)
import           "data-default"            Data.Default (Default, def)
import           "optparse-applicative"    Options.Applicative
import           "mtl"                     Control.Monad.State (execStateT, evalStateT)
import           "platform-types"          Platform.Types
import           "platform-dsl"            Platform.DSL
import qualified "platform-dsl"            Platform.DSL as DSL (test)
import           "platform-packaging"      Platform.Packaging
import           "dockerfile"              Data.Docker


main :: IO ()
main = do
    pwd <- getCurrentDirectory
    let dockerPath = pwd ++ "/DockerfileExample"
    dockerfileWrite dockerPath $ toDocker def
