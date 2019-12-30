module Atidot.Platform.Deployment.Interpreter.AMI where

import                        Prelude hiding (FilePath)
import           "text"       Data.Text (Text)
import qualified "text"       Data.Text as T
import           "free"       Control.Monad.Free
import           "mtl"        Control.Monad.State
import           "exceptions" Control.Monad.Catch (bracket)
import           "turtle"     Turtle
import           "uuid"       Data.UUID.V4 (nextRandom)
import           "directory"  System.Directory (doesFileExist)

import                        Atidot.Platform.Deployment.Interpreter.Utils
import                        Atidot.Platform.Deployment
import                        Atidot.Platform.Deployment.Interpreter.AMI.Template
import                        Atidot.Platform.Deployment.Interpreter.AMI.Types

-- // to login
-- // $ ssh ubuntu@<public_dns> -i ~/.ssh/terraform-keys2


runAMI :: AMIConfig -> DeploymentM a -> IO ()
runAMI config dep =
    bracket init'
            fini
            body
    where
        terraformAwsDep = renderProvider (_AMIConfig_terraformConfig config) allTemplates
        init' :: IO Text
        init' = do
            mktree terraformDepDir
            cd terraformDepDir
            output "example.tf" $ select $ textToLines terraformAwsDep
            procs "terraform" ["init"] stdin
            procs "terraform" ["apply", "-auto-approve"] stdin
            showOutput <- reduceShell $ inproc "terraform" ["show"] stdin
            sleep 8 -- wait for ssh on the remote machine to establish
            let publicDns = getPublicDns showOutput
            sshW publicDns ["sudo","apt","update"]
            sshW publicDns ["sudo","apt","install","docker.io","-y"]
            sshW publicDns ["sudo","usermod","-aG","docker","$USER"]
            sshW publicDns ["mkdir",rSecretsDir]
            return publicDns
        fini :: Text -> IO ()
        fini _publicDns = do
            -- save ami
            --showOutput <- reduceShell $ inproc "terraform" ["show"] stdin
            --let instanceId = getInstanceId showOutput
                --amiName = instanceId <> "-ami"
            --procs "aws" ["ec2", "create-image", "--instance-id", instanceId, "--name", amiName] stdin

          -- destroy all resources
            procs "terraform" ["destroy"] stdin
            cd ".."
            return ()
        body :: Text -> IO ()
        body publicDns = do
            _ <-  (runStateT (iterM (run publicDns) dep) config)
            return ()

        run :: Text -> Deployment (StateT AMIConfig IO a) -> StateT AMIConfig IO a
        run publicDns (Container containerName next) = do
            conf <- get
            let diskMappings = filter ((== Just containerName) . snd . snd ) $ _AMIConfig_mounts conf
                diskMappingsInDocker = concatMap (\(vol,(disk,_)) -> ["-v",vol <> ":" <> disk]) diskMappings
            lift $ sshW publicDns $ ["docker","run"] <> diskMappingsInDocker <> [containerName]
            next True

        run publicDns (Secret secretData next) = do
            isFile <- liftIO $ doesFileExist secretData
            if isFile then do
                -- path <- copy file into remote location
                -- conf <- get
                nuid <- liftIO nextRandom
                scpW publicDns (T.pack secretData) rSecretsDir
                --let fname = encodeString $ fromText rSecretsDir </> filename (decodeString secretData)
                -- add path to state
                --    conf' = conf{ _AMIConfig_secrets = _AMIConfig_secrets conf <> [(nuid,(secretData,Just fname,Nothing))]}
                next $ T.pack $ show nuid
            else do
                conf <- get
                nuid <- liftIO nextRandom
                -- store directly in the st ate
                let conf' = conf{ _AMIConfig_secrets = _AMIConfig_secrets conf <> [(nuid,(secretData,Nothing,Nothing))]}
                put conf'
                next $ T.pack $ show nuid

        run _ (Mount disk next) = do
            conf <- get
            nuid <- liftIO nextRandom
            let volume = T.pack $ "disk-" <> show nuid
            let containerName = case lookup volume $ _AMIConfig_mounts conf of
                    Just (_,cont) -> cont
                    Nothing       -> Nothing
                newMounts = (<> [(volume,(disk,containerName))]) $ filter ((/= volume) . fst) $ _AMIConfig_mounts conf
            put $ conf{_AMIConfig_mounts = newMounts}
            next volume
        run _ (AttachSecret _ _ next) = next
        run _ (AttachVolume _ _ next) = next
        run _ (Execute _ _ _ next) = next

sshW :: Text -> [Text] -> IO ()
sshW pdns cmd = procs "ssh" (["ubuntu@"<>pdns,"-o","StrictHostKeyChecking=no","-i","~/.ssh/terraform-keys2"] <> cmd) stdin

scpW :: MonadIO io => Text -> Text -> Text -> io ()
scpW pdns lcl rmt = procs "scp" ["-o","StrictHostKeyChecking=no","-i","~/.ssh/terraform-keys2",lcl, "ubuntu" <> "@" <> pdns <> ":" <> rmt] stdin
