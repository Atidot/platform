module Atidot.Platform.Deployment.Interpreter.AMI where

import                        Prelude hiding (FilePath)
import           "base"       Data.List
import           "text"       Data.Text (Text)
import qualified "text"       Data.Text as T
import qualified "text"       Data.Text.IO as T
import           "free"       Control.Monad.Free
import           "mtl"        Control.Monad.State
import           "exceptions" Control.Monad.Catch (MonadMask, bracket)
import           "turtle"     Turtle
import           "uuid"       Data.UUID.V4 (nextRandom)

import                        Atidot.Platform.Deployment
import                        Atidot.Platform.Deployment.Interpreter.AMI.Template
import                        Atidot.Platform.Deployment.Interpreter.AMI.Types
import Debug.Trace

lttrace x y = trace (x ++ ":" ++ show y) y
-- // to login
-- // $ ssh ubuntu@<public_dns> -i ~/.ssh/terraform-keys2

runAMI :: AMIConfig -> DeploymentM a -> IO ()
runAMI config dep =
    bracket init'
            fini
            body
    where
        terraformDepDir = "terraform_dep" :: FilePath
        terraformAwsDep = renderProvider (_AMIConfig_terraformConfig config) allTemplates
        getPublicDns = T.takeWhile (/= '"') . T.tail . T.dropWhile (/= '"') . snd . T.breakOn "public_dns"
        getInstanceId = T.takeWhile (/= '"') . T.tail . T.dropWhile (/= '"') . snd . T.breakOn "id" . T.takeWhile (/= '}') . T.dropWhile (/= '{') . snd . T.breakOn "aws_instance"
        reduceShell =  reduce $ Fold (<>) "" lineToText
        rSecretsDir = "/etc/.secrets"
        init' :: IO Text
        init' = do
            mktree terraformDepDir
            cd terraformDepDir
            output "example.tf" $ select $ textToLines terraformAwsDep
            procs "terraform" ["init"] stdin
            procs "terraform" ["apply", "-auto-approve"] stdin
            showOutput <- reduceShell $ inproc "terraform" ["show"] stdin
            sleep 6 -- wait for ssh on the remote machine to establish
            let publicDns = getPublicDns showOutput
            sshW publicDns ["sudo","apt","update"]
            sshW publicDns ["sudo","apt","install","docker.io","-y"]
            sshW publicDns ["sudo","usermod","-aG","docker","$USER"]
            sshW publicDns ["mkdir",rSecretsDir]
            return publicDns
        fini :: Text -> IO ()
        fini publicDns = do
            -- save ami
            showOutput <- reduceShell $ inproc "terraform" ["show"] stdin
            let instanceId = getInstanceId showOutput
                amiName = instanceId <> "-ami"
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
                diskMappingsInDocker = concatMap (\(vol,(disk,_)) -> ["-v",T.pack vol <> ":" <> T.pack disk]) diskMappings
            lift $ sshW publicDns $ ["docker","run"] <> diskMappingsInDocker <> [containerName]
            next True

        run publicDns (Secret secretData next) = do
            isFile <- testfile $ decodeString secretData
            if isFile then do
                -- path <- copy file into remote location
                conf <- get
                nuid <- liftIO nextRandom
                scpW publicDns (T.pack secretData) rSecretsDir
                let fname = encodeString $ fromText rSecretsDir </> filename (decodeString secretData)
                -- add path to state
                    conf' = conf{ _AMIConfig_secrets = _AMIConfig_secrets conf <> [(nuid,(secretData,Just fname,Nothing))]}
                next nuid
            else do
                conf <- get
                nuid <- liftIO nextRandom
                -- store directly in the state
                let conf' = conf{ _AMIConfig_secrets = _AMIConfig_secrets conf <> [(nuid,(secretData,Nothing,Nothing))]}
                put conf'
                next nuid

        run _ (Mount (Disk disk) (Volume volume) next) = do
            conf <- get
            let containerName = case lookup volume $ _AMIConfig_mounts conf of
                    Just (_,cont) -> cont
                    Nothing       -> Nothing
                newMounts = (<> [(volume,(disk,containerName))]) $ filter ((/= volume) . fst) $ _AMIConfig_mounts conf
            put $ conf{_AMIConfig_mounts = newMounts}
            next True

sshW :: Text -> [Text] -> IO ()
sshW pdns cmd = procs "ssh" (["ubuntu@"<>pdns,"-o","StrictHostKeyChecking=no","-i","~/.ssh/terraform-keys2"] <> cmd) stdin

scpW pdns lcl rmt = procs "scp" ["-o","StrictHostKeyChecking=no","-i","~/.ssh/terraform-keys2",lcl, "ubuntu" <> "@" <> pdns <> ":" <> rmt] stdin
