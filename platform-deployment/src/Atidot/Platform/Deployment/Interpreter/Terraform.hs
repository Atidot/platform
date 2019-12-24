{-# LANGUAGE FlexibleContexts #-}
module Atidot.Platform.Deployment.Interpreter.Terraform where

import           "text"       Data.Text (Text)
import qualified "text"       Data.Text as T
import           "exceptions" Control.Monad.Catch (MonadMask, bracket)
import           "free"       Control.Monad.Free
import           "mtl"        Control.Monad.State
import           "uuid"       Data.UUID.V4 (nextRandom)
import           "turtle"     Turtle

import       Atidot.Platform.Deployment.Interpreter.Utils
import       Atidot.Platform.Deployment.Interpreter.Terraform.Template
import       Atidot.Platform.Deployment


runTerraform :: TerraformExtendedConfig -> DeploymentM a -> IO ()
runTerraform config dep =
    bracket init'
            fini
            body
    where
        init' = do
            mktree terraformDepDir
            cd terraformDepDir
            return ()
        body _ = do
            (_,s) <- runStateT (iterM run dep) config
            output "example.tf" $ select $ textToLines $ renderTerraform s
            return ()
        fini _ = return ()

        run :: Deployment (StateT TerraformExtendedConfig IO a) -> StateT TerraformExtendedConfig IO a
        run (Container containerName next) = do
            conf <- get
            updateExec ["docker","pull", T.unpack containerName]
            next True
        run (Secret secretName next) = do
            -- pull secrets from aws vault
            addSecret secretName
            next $ T.pack secretName
        run (Mount folderName next) = do
            (devMapping, volId) <- getNextDisk
            addDisk devMapping volId
            let folderDir = "/" <> T.unpack folderName
            updateExec ["sudo","mkdir","-p",folderDir]
            updateExec ["sudo","mount", "/dev/" <> devMapping, folderDir]
            updateExec ["echo", "/dev/" <> devMapping, folderDir, "xfs", "defaults,nofail",  "0",  "2", "|", "sudo", "tee", "-a", "/etc/fstab"]
            updateExec ["sudo","cat","/etc/fstab"]
            next $ T.pack folderDir


updateExec cmd = modify $ \s -> s{ _TerraformExtendedConfig_instanceExec = _TerraformExtendedConfig_instanceExec s <> [foldl1 (\x y -> x <> " " <> y) cmd]}

addDisk diskName volume = modify $ \s -> s{ _TerraformExtendedConfig_disks = _TerraformExtendedConfig_disks s <> [(diskName,volume)]}

addSecret secretName = modify $ \s -> s{ _TerraformExtendedConfig_secrets = _TerraformExtendedConfig_secrets s <> [secretName]}

getNextDisk = do
    conf <- get
    let disks = _TerraformExtendedConfig_availableDisks conf
    if null disks
        then error "no more disks to attach"
        else do
            let (physDisk:rest) = disks
            put $ conf{ _TerraformExtendedConfig_availableDisks = rest}
            return physDisk