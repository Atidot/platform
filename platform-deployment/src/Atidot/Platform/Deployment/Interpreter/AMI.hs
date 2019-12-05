module Atidot.Platform.Deployment.Interpreter.AMI where

import                        Prelude hiding (FilePath)
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
        getPublicDns = undefined
        reduceShell =  reduce $ Fold (<>) "" lineToText
        init' :: IO Text
        init' = do
            mktree terraformDepDir
            cd terraformDepDir
            output "example.tf" $ select $ textToLines terraformAwsDep
            procs "terraform" ["init"] stdin
            procs "terraform" ["apply", "-auto-approve"] stdin
            applyOutput <- reduceShell $ inproc "terraform" ["show"] stdin
            let publicDns = applyOutput
            return publicDns
        fini :: Text -> IO ()
        fini publicDns = do
          -- save ami
          -- destroy all resources
            procs "terraform" ["destroy"] stdin
            cd ".."
            return ()
        body publicDns = do
            _ <-  (runStateT (iterM (run publicDns) dep) config)
            return ()

        run :: Text -> Deployment (StateT AMIConfig IO a) -> StateT AMIConfig IO a
        run _ (Container containerName next) = do
            conf <- get
            lift $ T.putStrLn $ renderProvider conf provider
            lift $ T.putStrLn $ renderProvider conf awsVpc
            liftIO $ putStrLn "some container cmd"
            next True
        run _ (Secret secretData next) = do
            liftIO $ putStrLn "some secret thingy"
            next ""
        run _ (Mount disk volume next) = do
            liftIO $ putStrLn "some storage mount"
            next True