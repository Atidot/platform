{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Platform.Terraform where

import "base"           Control.Monad.IO.Class (MonadIO, liftIO)
import "lens"           Control.Lens
import "text"           Data.Text (Text)
import "data-default"   Data.Default (Default, def)
import "mtl"            Control.Monad.State.Class (MonadState, gets)
import "exceptions"     Control.Monad.Catch (MonadMask, bracket)
import "free"           Control.Monad.Free
import "terraform-hs"   Language.Terraform.Core
import "terraform-hs"   Language.Terraform.Aws
import "platform-types" Platform.Types
import "platform-dsl"   Platform.DSL

---------
-- TODO: move to another file
type TerraformConfig = ()


data TerraformState
    = TerraformState
    { _terraformState_template :: !Template
    } deriving (Show, Eq)
makeLenses ''TerraformState

instance Default TerraformState where
    def = TerraformState (template $ Resources [])

---------
runTerraform :: (Monad m, MonadState TerraformState m, MonadMask m)
             => TerraformConfig
             -> Platform a
             -> m a
runTerraform config script
    = bracket init'
              fini
              body
    where
        init'  = return ()
        fini _ = return ()
        body _ = do
            result <- iterM run script
            return result

        run :: (Monad m, MonadState TerraformState m, MonadMask m)
             => PlatformCmd (m a)
             -> m a
        run (Container name return') = do
            undefined

        run (Connection (ContainerID name1)
                        (ContainerID name2)
                        return'
            ) = do
            return'
