{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}
module Platform.Packaging.Types
    ( OS(..)
    , User(..)
    , Entrypoint
    , ContainerEnv(..)
    ) where

import "base"         GHC.Generics (Generic)
import "base"         Data.Semigroup ((<>))
import "base"         Data.Typeable (Typeable)
import "base"         Data.Data (Data)
import "data-default" Data.Default (Default, def)
import "containers"   Data.Map.Strict (Map, empty)
import "aeson"        Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)

data OS
    = Ubuntu
    | RedHat
    | CentOS
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

instance ToJSON OS where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON OS where

data User
    = Root
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
