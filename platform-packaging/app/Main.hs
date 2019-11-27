{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           "base"                    System.IO (stdin)
import           "base"                    Data.Semigroup ((<>))
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

data InFile = InFile
    { location :: !FilePath }
    deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

infile :: Parser InFile
infile = InFile
      <$> strOption
          ( long "infile"
         <> metavar "INPUT_FILE"
         <> help "An input Python file." )

main :: IO ()
main = catDocker =<< execParser opts
    where opts = info (infile <**> helper)
                 (fullDesc 
                 <> progDesc "Print a Dockerfile that can run the input Python file." 
                 <> header   "platform-packaging - currently a simplified test version.")

catDocker :: InFile -> IO ()
catDocker (InFile loc) = do
    modulesToInstall <- runPythonImports loc
    print modulesToInstall
