{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import           "base"                    Data.Semigroup ((<>))
import           "base"                    System.IO (stdin)
import           "base"                    Data.Typeable (Typeable)
import           "base"                    Data.Data (Data)
import           "base"                    GHC.Generics (Generic)
import           "base"                    System.IO
import qualified "bytestring"              Data.ByteString.Lazy.Char8 as B8 (putStrLn)
import           "data-default"            Data.Default (Default, def)
import           "dockerfile"              Data.Docker
import           "containers"              Data.Map.Strict (empty)
import           "text"                    Data.Text (unpack)
import           "mtl"                     Control.Monad.State (execStateT, evalStateT)
import           "optparse-applicative"    Options.Applicative (Parser, strOption, long, metavar, help, header, fullDesc, helper, progDesc, info, execParser, (<**>))
import           "directory"               System.Directory (getCurrentDirectory)
import           "platform-types"          Platform.Types
import           "platform-dsl"            Platform.DSL
import qualified "platform-dsl"            Platform.DSL as DSL (test)
import           "platform-packaging"      Platform.Packaging
import           "platform-packaging"      Platform.Packaging.PythonImports

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
    handle <- openFile loc ReadMode
    contents <- hGetContents handle
    pipModulesForInstall <- fmap (map (unpack . _pyPkg_name . snd) . fst) $ runPythonImports contents
    let env = ContainerEnv 
              Ubuntu 
              [User "atidot"] 
              "ubuntu:latest" 
              [("pip install -q", pipModulesForInstall)]
              empty
              []
              Nothing
              Nothing
    putStrLn . dockerfile . toDocker $ env
