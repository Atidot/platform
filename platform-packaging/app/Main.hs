{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import           "base"                     Data.Semigroup ((<>))
import           "base"                     System.IO (stdin)
import           "base"                     Data.Typeable (Typeable)
import           "base"                     Data.Data (Data)
import           "base"                     GHC.Generics (Generic)
import           "base"                     System.IO
import qualified "bytestring"               Data.ByteString.Lazy.Char8 as B8 (putStrLn)
import           "data-default"             Data.Default (Default, def)
import           "dockerfile"               Data.Docker
import           "containers"               Data.Map.Strict (empty)
import           "text"                     Data.Text (unpack)
import           "mtl"                      Control.Monad.State (execStateT, evalStateT)
import           "optparse-generic"         Options.Generic
import           "directory"                System.Directory (getCurrentDirectory)
import           "language-python"          Language.Python.Common.Pretty (Pretty, pretty)
import           "language-python"          Language.Python.Common.PrettyAST ()
import           "platform-types"           Platform.Types
import           "platform-dsl"             Platform.DSL
import qualified "platform-dsl"             Platform.DSL as DSL (test)
import           "platform-packaging"       Platform.Packaging
import           "platform-packaging"       Platform.Packaging.PythonImports
import           "platform-packaging-types" Platform.Packaging.Types

data CLI
    = PyToDocker
    { infile :: String
    }
    | CatDocker
    { infile :: String
    }
    | PrintAST
    { infile :: String
    } deriving (Generic, Show)

instance ParseRecord CLI

main :: IO ()
main = getRecord "Platform Packaging" >>= runScript

runScript :: CLI -> IO ()
runScript p@PyToDocker{} = ioWrapper pyToDocker $ infile p
runScript a@PrintAST{}   = ioWrapper printAST   $ infile a

ioWrapper :: (String -> IO ()) -> String -> IO ()
ioWrapper f inFile = do
    handle <- openFile inFile ReadMode
    contents <- hGetContents handle
    f contents

pyToDocker :: String -> IO ()
pyToDocker module' = do
    pipModulesForInstall <- fmap (map (unpack . _pyPkg_name . snd) . fst)
                          . runPythonImports
                          $ module'
    let env = ContainerEnv
              Ubuntu
              [User "atidot"]
              "ubuntu:latest"
              [("pip install -q", pipModulesForInstall)]
              empty
              []
              Nothing
              Nothing
    putStrLn . dockerfile
             . toDocker
             $ env

printAST :: String -> IO ()
printAST module' = do
    ast <- getAST module'
    putStrLn . show $ ast

printImports :: String -> IO ()
printImports s = do
    imports <- runPythonImports s
    let pkgs = map (unpack . _pyPkg_name . snd) . fst $ imports
    putStrLn . unlines $ pkgs
