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
        terraformAwsDep = renderProvider config allTemplates
        getPublicDns = T.takeWhile (/= '"') . T.tail . T.dropWhile (/= '"') . snd . T.breakOn "public_dns"
        reduceShell =  reduce $ Fold (<>) "" lineToText
        init' :: IO Text
        init' = do
            mktree terraformDepDir
            cd terraformDepDir
            output "example.tf" $ select $ textToLines terraformAwsDep
            procs "terraform" ["init"] stdin
            procs "terraform" ["apply", "-auto-approve"] stdin
            applyOutput <- reduceShell $ inproc "terraform" ["show"] stdin
            sleep 6 -- wait for ssh on the remote machine to establish
            let publicDns = getPublicDns applyOutput
            sshW publicDns ["sudo","apt","update"]
            sshW publicDns ["sudo","apt","install","docker.io","-y"]
            sshW publicDns ["sudo","usermod","-aG","docker","$USER"]
            return publicDns
        fini :: Text -> IO ()
        fini publicDns = do
          -- save ami
          -- destroy all resources
            procs "terraform" ["destroy"] stdin
            cd ".."
            return ()
        body :: Text -> IO ()
        body publicDns = do
            --let [knownHostFp,pubkey,privkey] = map ("~/.ssh/" <>) ["known_hosts","terraform-keys2.pub","terraform-keys2"]
            --putStrLn "------------------------------------------"
            --putStrLn hostname
            --sshW publicDns ["touch","hello"]
            _ <-  (runStateT (iterM (run $ sshW publicDns) dep) config)
            return ()

        run :: ([Text] -> IO ()) -> Deployment (StateT AMIConfig IO a) -> StateT AMIConfig IO a
        run sshW' (Container containerName next) = do
            conf <- get
            lift $ sshW' ["docker","run",containerName]
            next True
        run _ (Secret secretData next) = do
            liftIO $ putStrLn "some secret thingy"
            next ""
        run _ (Mount disk volume next) = do
            liftIO $ putStrLn "some storage mount"
            next True

sshW :: Text -> [Text] -> IO ()
sshW pdns cmd = procs "ssh" (["ubuntu@"<>pdns,"-o","StrictHostKeyChecking=no","-i","~/.ssh/terraform-keys2"] ++ cmd) stdin
