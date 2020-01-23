{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
module Platform.Packaging where

import "base"                     Data.Foldable (foldl')
import "base"                     Data.List (intercalate)
import "base"                     Data.Typeable (Typeable)
import "base"                     Data.Data (Data)
import "base"                     GHC.Generics (Generic)
import "lens"                     Control.Lens hiding (from)
import "containers"               Data.Map.Strict (assocs, empty)
import "dockerfile"               Data.Docker
import "text"                     Data.Text (unpack)
import "regex-pcre"               Text.Regex.PCRE hiding (empty)
import "platform-types"           Platform.Types
import "platform-packaging-types" Platform.Packaging.Types
import                            Platform.Packaging.PythonImports

makeLenses ''ContainerEnv

toDocker :: ContainerEnv -> Docker ()
toDocker (ContainerEnv os users img preInstallationCmds pkgs environment runCmds entrypoint' command) = do
    from img
    mapM_ run                   preInstallationCmds
    mapM_ (uncurry installPkgs) pkgs
    mapM_ (makeUser os)         users
    mapM_ (uncurry env)         (assocs environment)
    mapM_ run                   runCmds
    maybe (return ()) (flip entrypoint []) entrypoint'
    maybe (return ()) cmd                  command

toDockerString :: ContainerEnv -> String
toDockerString = dockerfile . toDocker

pythonToContainerEnv :: String
                     -> ContainerEnv
                     -> IO ContainerEnv
pythonToContainerEnv module' env = do
    pipModulesForInstall <- fmap (map (unpack . _pyPkg_name . snd) . fst)
                          $ runPythonImports module'
    let installPythonEnv = env & containerEnv_installations <>~ [("pip3 install -q", pipModulesForInstall)]
    return installPythonEnv

pythonToContainerEnvDefault :: String
                            -> IO ContainerEnv
pythonToContainerEnvDefault module' = pythonToContainerEnv module' testingEnv

pythonToDockerString :: String
                     -> ContainerEnv
                     -> IO String
pythonToDockerString module' env = fmap (dockerfile . toDocker) $ pythonToContainerEnv module' env

pythonToDockerStringDefault :: String
                            -> IO String
pythonToDockerStringDefault module' = fmap (dockerfile . toDocker) $ pythonToContainerEnvDefault module'

testingEnv :: ContainerEnv
testingEnv = ContainerEnv Ubuntu
                          [User "atidot"]
                          "ubuntu:latest"
                          [ "apt-get update"
                          , "apt-get -y install sudo"]
                          --, "echo \"atidot:atidot\" | chpasswd && adduser atidot sudo"]
                          [("sudo apt-get -y install", ["python3", "python3-pip"])]
                          empty
                          []
                          Nothing
                          Nothing

installPkgs :: String
            -> [String]
            -> Docker ()
installPkgs installer []   = return ()
installPkgs installer pkgs = run $ installation =~ ("\\A(.|\\n|\\r|\\f)*(?=\\s*\\\\\\Z)" :: String)
    where installation = foldl' (++) (installer ++ endl1) (map instLine pkgs)
          instLine pkg = "    " ++ pkg ++ endl pkg
          endl pkg     = replicate (maxLength - paddedLength pkg) ' ' ++ " \\\n"
          paddedLine1  = "RUN " ++ installer ++ " \\"
          endl1        = replicate (maxLength - length paddedLine1) ' ' ++ " \\\n"
          maxLength    = maximum $ map length (paddedLine1 : map pad pkgs)
          paddedLength = length . pad
          pad pkg      = "    " ++ pkg ++ " \\"

addUserProgram :: OS -> String
addUserProgram Ubuntu = "adduser"
addUserProgram CentOS = "adduser"
addUserProgram RedHat = "useradd"

makeUser :: OS
         -> User
         -> Docker ()
makeUser _  Root         = return ()
makeUser os (User uname) = do
    run (addUserProgram os ++ " " ++ uname)
    user uname
    workdir ("/home/" ++ uname)
    env "PATH" "/home/atidot/.local/bin:${PATH}" --TODO: look up the home directory.
