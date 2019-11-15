{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
module Platform.Packaging where

import "lens" Control.Lens
import "base" Data.Foldable (foldl')
import "base" Data.Semigroup (Semigroup, (<>))
import "data-default" Data.Default (Default, def)
import "containers" Data.Map.Strict (Map)
import "text" Data.Text (Text)
import "platform-types" Platform.Types
import "shelly" Shelly
import "dockerfile" Data.Docker (Docker)

data ContainerEnv
    = ContainerEnv
        { _containerEnv_image :: Image
        , _containerEnv_installations :: ![(Text, [Text])]
        , _containerEnv_env :: !(Map Text Text)
        , _containerEnv_command :: ![Text]
        } deriving (Show, Eq)
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

toDocker :: ContainerEnv -> Docker a
toDocker ContainerEnv image instList env command = do
    from image
    foldl' (>>) (return ()) installations
        where installations = map dockerInstall instList
              dockerInstall (installer, programs) = undefined
