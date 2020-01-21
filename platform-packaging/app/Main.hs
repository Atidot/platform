{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import           "base"                     Data.Semigroup ((<>))
import           "base"                     Data.List (isSuffixOf)
import           "base"                     Data.Typeable (Typeable)
import           "base"                     Data.Data (Data)
import           "base"                     GHC.Generics (Generic)
import           "base"                     System.IO
import           "base"                     Control.Monad (filterM)
import           "aeson"                    Data.Aeson (Value, decode)
import qualified "bytestring"               Data.ByteString.Lazy.Char8 as B8 (pack)
import           "data-default"             Data.Default (Default, def)
import           "dockerfile"               Data.Docker (Docker, dockerfile)
import           "containers"               Data.Map.Strict (empty)
import qualified "text"                     Data.Text.IO as TIO
import qualified "text"                     Data.Text as T
import           "text"                     Data.Text (unpack)
import           "mtl"                      Control.Monad.State (execStateT, evalStateT)
import           "optparse-generic"         Options.Generic
import           "directory"                System.Directory (getCurrentDirectory, listDirectory, doesDirectoryExist, doesFileExist)
import           "language-python"          Language.Python.Common.Pretty (Pretty, pretty)
import           "language-python"          Language.Python.Common.PrettyAST ()
import           "platform-types"           Platform.Types
import           "platform-dsl"             Platform.DSL
import qualified "platform-dsl"             Platform.DSL as DSL (test)
import           "platform-packaging-types" Platform.Packaging.Types
import           "platform-packaging"       Platform.Packaging
import           "platform-packaging"       Platform.Packaging.PythonImports

data CLI
    = PyToDocker
    { input :: String
    , env :: Maybe String -- TODO: decide whether to use Data.Aeson.fromJSON for taking ContainerEnv inputs
    , outfile :: Maybe String -- if this is Nothing, just pipe to stdout
    }
    | CatDocker
    { input :: String
    }
    | PrintAST
    { infile :: String
    } deriving (Generic, Show)

instance ParseRecord CLI

main :: IO ()
main = getRecord "Platform Packaging" >>= runScript

runScript :: CLI -> IO ()
runScript p@PyToDocker{} = do
    let fp = input p
    let env' = fmap B8.pack (env p) >>= decode
    isDir <- doesDirectoryExist fp
    isFile <- doesFileExist fp
    if (isDir && isFile) || (not isDir && not isFile) -- TODO: abstract this pattern
       then error "Impossible file state" -- TODO: call on proper throwM function
       else return ()
    if isDir
       then do
            modules <- fmap unpack . recursivelyConcatenate $ fp
            pipeTo (outfile p) . dockerfile =<< pythonToDockerDefault modules env'
       else ioWrapper (\instring -> pipeTo (outfile p) . dockerfile =<< pythonToDockerDefault instring env') fp
    where
        pipeTo Nothing     = putStrLn
        pipeTo (Just file) = writeFile file
runScript a@PrintAST{} = ioWrapper printAST $ infile a

ioWrapper :: (String -> IO ()) -> FilePath -> IO ()
ioWrapper f inFile = do
    handle <- openFile inFile ReadMode
    contents <- hGetContents handle
    f contents

printAST :: String -> IO ()
printAST module' = maybe (putStrLn "getAST failed") (putStrLn . show) (getAST module')

printImports :: String -> IO ()
printImports s = do
    imports <- runPythonImports s
    let pkgs = map (unpack . _pyPkg_name . snd) . fst $ imports
    putStrLn . unlines $ pkgs
