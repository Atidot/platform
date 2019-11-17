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
import "dockerfile"     Data.Docker (Docker, from, run, cmd)
import "platform-types" Platform.Types

data ContainerEnv
    = ContainerEnv
        { _containerEnv_image :: !String
        , _containerEnv_installations :: ![(String, [String])]
        , _containerEnv_env :: !(Map String String)
        , _containerEnv_command :: ![String]
        } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)
makeLenses ''ContainerEnv

instance Default ContainerEnv where
    def = ContainerEnv "ubuntu:latest" [] empty []

-- The semigroup operator for ContainerEnv prefers values from the right operand.
-- I.e. "new" environment variables override old ones, and the new command
-- overrides the old command. (The "newer" ContainerEnv is the right-hand one.)
instance Semigroup ContainerEnv where
    (<>) (ContainerEnv image1 insts1 env1 command1) 
         (ContainerEnv image2 insts2 env2 command2)
        = ContainerEnv image2 (insts1 <> insts2) (env2 <> env1) command2
        -- Note: `env2 <> env1` is not a typo, since Data.Map prefers the left value.

toDocker :: ContainerEnv -> Docker ()
toDocker (ContainerEnv image instList env command) = do
    from image
    iterateRun $ map (\(k, v) -> "export " ++ k ++ "=" ++ v) (assocs env)
    -- Some kind of iterateRun to install the packages required
    cmd command

iterateRun :: [String] -> Docker ()
iterateRun xs = foldl' (>>) (return ()) (map run xs)
