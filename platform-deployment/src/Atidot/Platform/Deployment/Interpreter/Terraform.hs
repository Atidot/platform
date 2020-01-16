{-# LANGUAGE FlexibleContexts #-}
module Atidot.Platform.Deployment.Interpreter.Terraform where

import qualified "text"       Data.Text as T
import           "base"       Data.Maybe
import           "extra"      Data.Tuple.Extra
import           "directory"  System.Directory
import           "exceptions" Control.Monad.Catch
import           "free"       Control.Monad.Free
import           "mtl"        Control.Monad.State
import           "turtle"     Turtle hiding (x, s, d,FilePath, fp)
import           "filepath"   System.FilePath
import qualified "containers" Data.Map as M

import       Atidot.Platform.Deployment.Interpreter.Utils
import       Atidot.Platform.Deployment.Interpreter.Terraform.Template
import       Atidot.Platform.Deployment hiding (VolumeName, SecretName, Name, FolderDir)


runTerraform :: TerraformExtendedConfig
             -> DeploymentM a
             -> IO ()
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
            output "aws_cli_config.tf" $ select $ textToLines awsConfigVars
            procs "terraform" ["init"] stdin
            return ()
        fini _ = return ()

        run :: Deployment (StateT TerraformExtendedConfig IO a)
            -> StateT TerraformExtendedConfig IO a
        run (Container containerName next) = do
            pathExists <- lift $ doesPathExist $ T.unpack containerName
            if pathExists
                then do
                    let fp = T.unpack containerName
                        containerName' = takeBaseName fp
                    addDockerFromPath containerName' $ Just fp
                    next $ T.pack containerName'
                else do
                    addDocker $ T.unpack containerName
                    next containerName
        run (Secret secretName next) = do
            -- pull secrets from aws vault
            secretRetrivalFailed secretName $ shells ("aws secretsmanager get-secret-value --secret-id " <> secretName <> " > /dev/null 2>&1") stdin
            addSecret $ T.unpack secretName
            next $ T.pack $ secretifyName $ T.unpack secretName
        run (Mount folderName next) = do
            (devMapping, volId) <- getNextDisk
            addDisk devMapping volId
            let folderDir = "/" <> T.unpack folderName
            updatePrep ["sudo","mkdir","-p",folderDir]
            updatePrep ["sudo","mount", "/dev/" <> devMapping, folderDir]
            updatePrep ["echo", "/dev/" <> devMapping, folderDir, "xfs", "defaults,nofail",  "0",  "2", "|", "sudo", "tee", "-a", "/etc/fstab"]
            updatePrep ["sudo","cat","/etc/fstab"]
            next $ T.pack folderDir
        run (AttachSecret secretName name next) = do
            conf <- get
            let containerMapping = case M.lookup (T.unpack name) (_TerraformExtendedConfig_dockers conf) of
                    Just x -> x
                    Nothing -> error $ "container '" <> T.unpack name <> "' does not exist"
                secretsForAttachment = fst3 containerMapping
                secrets = _TerraformExtendedConfig_secrets conf
                secretName' = T.unpack secretName
            unless ( secretName' `elem` map secretifyName secrets) $ error $ "secret '" <> secretName' <> "' not found for attachment"
            when ( secretName' `elem` secretsForAttachment) $ error $ "secret '" <> secretName' <> "' is already attached to container '" <> T.unpack name <> "'"
            attachDockerSecret (T.unpack name) secretName'
            next
        run (AttachVolume folderDir name next) = do
            attachDockerFolder (T.unpack name) (T.unpack folderDir)
            next
        run (Execute containerEngineArgs name containerArgs next) = do
            conf <- get
            let name' = T.unpack name
                (secrets,dirs,_) = fromMaybe ([],[],Nothing) $ M.lookup name' $ _TerraformExtendedConfig_dockers conf
                dirs' = map (\d -> ["-v",d <> ":" <> d]) dirs
                secrets' = map (\s -> ["-e",s <> "=$" <> s]) secrets
                cmd = ["docker","run"] ++ map T.unpack containerEngineArgs ++ concat dirs' ++ concat secrets' ++ [name'] ++ map T.unpack containerArgs
            updateExec cmd
            next

updatePrep :: (MonadState TerraformExtendedConfig m, Foldable t)
           => t [Char]
           -> m ()
updatePrep cmd = modify $ \s -> s{ _TerraformExtendedConfig_instancePrep = _TerraformExtendedConfig_instancePrep s <> addCmd cmd}

updateExec :: (MonadState TerraformExtendedConfig m, Foldable t)
           => t [Char]
           -> m ()
updateExec cmd = modify $ \s -> s{ _TerraformExtendedConfig_instanceExec = _TerraformExtendedConfig_instanceExec s <> addCmd cmd}

addCmd :: (Foldable t, Semigroup a, IsString a)
       => t a
       -> [a]
addCmd cmd = [foldl1 (\x y -> x <> " " <> y) cmd]

attachDockerFolder :: MonadState TerraformExtendedConfig m
                   => Name
                   -> FolderDir
                   -> m ()
attachDockerFolder name folderDir = modify $ \s ->  case M.lookup name (_TerraformExtendedConfig_dockers s) of
    Nothing -> error $ "attachDockerFolder: docker '" ++ show name ++ "' not found"
    Just _ -> s{ _TerraformExtendedConfig_dockers =
                    M.insertWith
                        updateDockerVal
                        name
                        ([],[folderDir],Nothing)
                        (_TerraformExtendedConfig_dockers s)
               }

attachDockerSecret :: MonadState TerraformExtendedConfig m
                   => Name
                   -> SecretName
                   -> m ()
attachDockerSecret name sec = modify $ \s ->  case M.lookup name (_TerraformExtendedConfig_dockers s) of
    Nothing -> error $ "attachDockerSecret: docker '" ++ show name ++ "' not found"
    Just _ -> s{ _TerraformExtendedConfig_dockers =
                    M.insertWith
                        updateDockerVal
                        name
                        ([sec],[],Nothing) --
                        (_TerraformExtendedConfig_dockers s)
               }

updateDockerVal :: ([a1], [a2], c1)
                -> ([a1], [a2], c2)
                -> ([a1], [a2], c2)
updateDockerVal newv oldv = (fst3 newv ++ fst3 oldv,snd3 newv ++ snd3 oldv,thd3 oldv)

addDocker :: MonadState TerraformExtendedConfig m
              => Name
              -> m ()
addDocker name = addDockerFromPath name Nothing

addDockerFromPath :: MonadState TerraformExtendedConfig m
                  => Name
                  -> Maybe FilePath
                  -> m ()
addDockerFromPath name mFp = modify $ \s ->
    if M.member name (_TerraformExtendedConfig_dockers s)
    then s
    else s{ _TerraformExtendedConfig_dockers =
                M.insert
                    name
                    ([],[],mFp)
                    (_TerraformExtendedConfig_dockers s)
          }

addDisk :: MonadState TerraformExtendedConfig m
        => DeviceName
        -> VolumeName
        -> m ()
addDisk diskName volume = modify $ \s -> s{ _TerraformExtendedConfig_disks = _TerraformExtendedConfig_disks s <> [(diskName,volume)]}


addSecret :: MonadState TerraformExtendedConfig m
          => SecretName
          -> m ()
addSecret secretName = modify $ \s -> if elem secretName $ _TerraformExtendedConfig_secrets s
    then error $ "secret '" <> secretName <> "' already exists"
    else s{ _TerraformExtendedConfig_secrets = _TerraformExtendedConfig_secrets s <> [secretName]}

getNextDisk :: StateT TerraformExtendedConfig IO (DeviceName, VolumeName)
getNextDisk = do
    conf <- get
    let disks = _TerraformExtendedConfig_availableDisks conf
    if null disks
        then error "no more disks to attach"
        else do
            let (physDisk:rest) = disks
            put $ conf{ _TerraformExtendedConfig_availableDisks = rest}
            return physDisk


secretRetrivalFailed :: MonadCatch m
                     => Text
                     -> m a
                     -> m a
secretRetrivalFailed secretName action = action `catch` (\(_ :: ShellFailed) -> error $ "secret '" <> T.unpack secretName <> "' not found")