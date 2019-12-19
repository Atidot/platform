{-# LANGUAGE FlexibleContexts #-}
module Atidot.Platform.Deployment.Interpreter.Terraform where

import           "text"       Data.Text (Text)
import qualified "text"       Data.Text as T
import           "exceptions" Control.Monad.Catch (MonadMask, bracket)
import           "mtl"        Control.Monad.State
import           "uuid"       Data.UUID.V4 (nextRandom)

import       Atidot.Platform.Deployment.Interpreter.Terraform.Template
import       Atidot.Platform.Deployment


runTerraform :: TerraformExtendedConfig -> DeploymentM a -> IO ()
runTerraform config dep =
    bracket init'
            fini
            body
    where
        init' = return ()
        body  = undefined
        fini  = undefined

        run :: Text -> Deployment (StateT TerraformExtendedConfig IO a) -> StateT TerraformExtendedConfig IO a
        run publicDns (Container containerName next) = do
            conf <- get
            updateExec $ foldl1 (<>) ["docker","pull", T.unpack containerName]
            next True
        run publicDns (Secret secretName next) = do
            -- pull secrets from aws vault
            addSecret secretName
            next $ T.pack secretName
        run _ (Mount disk next) = do
            conf <- get
            nuid <- liftIO nextRandom
            let volume = "disk-" <> show nuid
            addDisk (T.unpack disk) volume
            next $ T.pack volume


updateExec cmd = modify $ \s -> s{ _TerraformExtendedConfig_instanceExec = _TerraformExtendedConfig_instanceExec s <> [cmd]}

addDisk diskName volume = modify $ \s -> s{ _TerraformExtendedConfig_disks = _TerraformExtendedConfig_disks s <> [(diskName,volume)]}

addSecret secretName = modify $ \s -> s{ _TerraformExtendedConfig_secrets = _TerraformExtendedConfig_secrets s <> [secretName]}