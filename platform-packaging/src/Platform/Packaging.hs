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
import "aeson"          Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import "data-default"   Data.Default (Default, def)
import "containers"     Data.Map.Strict (Map, empty, assocs)
import "text"           Data.Text (Text)
import "dockerfile"     Data.Docker
import "platform-types" Platform.Types

data OS = Ubuntu
        | RedHat
        | CentOS
        deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

instance ToJSON OS where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON OS where

data User = Root
          | User { _user_name :: !String }
          deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance ToJSON User where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON User where

instance Default User where
    def = Root

type Entrypoint = String

data ContainerEnv
    = ContainerEnv
        { _containerEnv_OS :: !OS
        , _containerEnv_users :: ![User]
        , _containerEnv_image :: !String
        , _containerEnv_installations :: ![(String, [String])]
        , _containerEnv_env :: !(Map String String)
        , _containerEnv_runCmds :: ![String]
        , _containerEnv_entrypoint :: !(Maybe Entrypoint)
        , _containerEnv_command :: !(Maybe [String])
        } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)
makeLenses ''ContainerEnv

instance Default ContainerEnv where
    def = ContainerEnv Ubuntu
                       [Root]
                       "ubuntu:latest"
                       []
                       empty
                       []
                       Nothing
                       Nothing

instance ToJSON ContainerEnv where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ContainerEnv where

-- The semigroup operator for ContainerEnv prefers values from the right operand.
-- I.e. "new" environment variables override old ones, and the new command
-- overrides the old command. (The "newer" ContainerEnv is the right-hand one.)
instance Semigroup ContainerEnv where
    (<>) (ContainerEnv os1 users1 image1 insts1 env1 runs1 entry1 command1)
         (ContainerEnv os2 users2 image2 insts2 env2 runs2 entry2 command2)
        = ContainerEnv os2
                       (users1 <> users2)
                       image2
                       (insts1 <> insts2)
                       (env2 <> env1) -- Map prefers the left value
                       (runs1 <> runs2)
                       entry2
                       command2

toDocker :: ContainerEnv -> Docker ()
toDocker (ContainerEnv os users img pkgs environment runCmds entrypoint' command) = do
    from img
    mapM_ (makeUser os)         users
    mapM_ (uncurry installPkgs) pkgs
    mapM_ (uncurry env)         (assocs environment)
    mapM_ run                   runCmds
    maybe (return ()) (flip entrypoint []) entrypoint'
    maybe (return ()) cmd                  command

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
