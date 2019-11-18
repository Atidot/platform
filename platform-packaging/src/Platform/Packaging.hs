{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
module Platform.Packaging where

import "base"           Data.Foldable (foldl')
import "base"           GHC.Generics (Generic)
import "base"           Data.Typeable (Typeable)
import "base"           Data.Data (Data)
import "base"           Data.Semigroup (Semigroup, (<>))
import "lens"           Control.Lens hiding (from)
import "data-default"   Data.Default (Default, def)
import "containers"     Data.Map.Strict (Map, empty, assocs)
import "text"           Data.Text (Text)
import "dockerfile"     Data.Docker
import "platform-types" Platform.Types

data OS = Ubuntu
        | RedHat
        | CentOS
        deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

data User = Root
          | User { _user_name :: !Text }
          deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance Default User where
    def = Root

type Entrypoint = String

data ContainerEnv
    = ContainerEnv
        { _containerEnv_OS :: !OS
        , _containerEnv_users :: ![User]
        , _containerEnv_image :: !String
        , _containerEnv_installations :: ![(User, String, [String])]
        , _containerEnv_env :: !(Map String String)
        , _containerEnv_runCmds :: ![String]
        , _containerEnv_entrypoint :: !(Maybe Entrypoint)
        , _containerEnv_command :: !(Maybe String)
        } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)
makeLenses ''ContainerEnv

instance Default ContainerEnv where
    def = ContainerEnv Ubuntu 
                       [Root] 
                       "ubuntu:latest" 
                       [] 
                       empty 
                       Nothing
                       Nothing

-- The semigroup operator for ContainerEnv prefers values from the right operand.
-- I.e. "new" environment variables override old ones, and the new command
-- overrides the old command. (The "newer" ContainerEnv is the right-hand one.)
instance Semigroup ContainerEnv where
    (<>) (ContainerEnv image1 insts1 env1 command1) 
         (ContainerEnv image2 insts2 env2 command2)
        = ContainerEnv image2 (insts1 <> insts2) (env2 <> env1) command2
        -- Note: `env2 <> env1` is not a typo, since Data.Map prefers the left value.

toDocker :: ContainerEnv -> Docker ()
toDocker env = do
    from $ _containerEnv_image env
    foreach makeUser $ _containerEnv_users env
    foreach installPkgs' $ _containerEnv_installations env
    foreach export $ _containerEnv_env env
    foreach run $ _containerEnv_runCmds env
    doIfJust entrypoint $ _containerEnv_entrypoint env
    doIfJust cmd $ _containerEnv_command env
        where installPkgs' (installer, pkgs) = installPkgs installer pkgs
              doIfJust f Nothing = return ()
              doIfJust f (Just x) = f x

foreach :: (a -> Docker ()) 
        -> [a] 
        -> Docker ()
foreach f xs = foldl' (>>) (return ()) (map f xs)

-- TODO: Generalize this to take any line1
installPkgs :: String
            -> [String] 
            -> Docker ()
installPkgs installer pkgs = run [installation]
    where installation = foldl' (++) (installer ++ endl1) (map instLine pkgs)
          instLine pkg = "    " ++ pkg ++ endl pkg
          endl pkg     = replicate (maxLength - paddedLength pkg) ' ' ++ " \\\n"
          paddedLine1  = "RUN " ++ installer ++ " \\"
          endl1        = replicate (maxLength - length paddedLine1) ' ' ++ " \\\n"
          maxLength    = max $ map length (paddedLine1 : map pad pkgs)
          paddedLength = length . pad
          pad pkg      = "    " ++ pkg ++ " \\"

addUserProgram :: OS -> String
addUserProgram Ubuntu = "adduser"
addUserProgram CentOS = "adduser"
addUserProgram RedHat = "useradd"

makeUser :: OS 
         -> String 
         -> Docker ()
makeUser os uname = do
    run [addUserProgram os, uname]
    user uname
    workdir ("/home/" ++ uname)
    env "PATH" "/home/atidot/.local/bin:${PATH}"
