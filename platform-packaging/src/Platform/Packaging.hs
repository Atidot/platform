{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
module Platform.Packaging where

import "base"                     Data.Foldable (foldl')
import "base"                     GHC.Generics (Generic)
import "base"                     Data.Typeable (Typeable)
import "base"                     Data.Data (Data)
import "lens"                     Control.Lens hiding (from)
import "containers"               Data.Map.Strict (assocs)
import "dockerfile"               Data.Docker
import "text"                     Data.Text (unpack)
import "platform-types"           Platform.Types
import "platform-packaging-types" Platform.Packaging.Types
import                            Platform.Packaging.PythonImports

makeLenses ''ContainerEnv

toDocker :: ContainerEnv -> Docker ()
toDocker (ContainerEnv os users img pkgs environment runCmds entrypoint' command) = do
    from img
    mapM_ (makeUser os)         users
    mapM_ (uncurry installPkgs) pkgs
    mapM_ (uncurry env)         (assocs environment)
    mapM_ run                   runCmds
    maybe (return ()) (flip entrypoint []) entrypoint'
    maybe (return ()) cmd                  command

pythonToDocker :: String
               -> ContainerEnv
               -> IO (Docker ())
pythonToDocker module' env = do
    pipModulesForInstall <- fmap (map (unpack . _pyPkg_name . snd) . fst)
                          $ runPythonImports module'
    return $ toDocker env -- TODO modify env according to the information in pipModulesForInstall

pythonToDockerDefault :: String
                      -> Maybe ContainerEnv
                      -> IO (Docker ())
pythonToDockerDefault module' env = maybe def' applyToDocker env
  where
      def' = undefined
      applyToDocker = undefined

installPkgs :: String
            -> [String]
            -> Docker ()
installPkgs installer []   = return ()
installPkgs installer pkgs = run installation
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
