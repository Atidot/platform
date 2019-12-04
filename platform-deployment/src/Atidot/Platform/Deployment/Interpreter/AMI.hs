module Atidot.Platform.Deployment.Interpreter.AMI where

import "text" Data.Text (Text)
import qualified "text" Data.Text as T
import qualified "text" Data.Text.IO as T
import "free"           Control.Monad.Free
import                  Atidot.Platform.Deployment
import "mtl"            Control.Monad.State


import "exceptions"     Control.Monad.Catch (MonadMask, bracket)
import Atidot.Platform.Deployment.Interpreter.AMI.Template
import Atidot.Platform.Deployment.Interpreter.AMI.Types



-- // to login
-- // $ ssh ubuntu@<public_dns> -i ~/.ssh/terraform-keys2



runAMI :: AMIConfig -> DeploymentM a -> IO ()
runAMI config dep = do
    _ <-  (runStateT (iterM run dep) config)
    return ()
    -- bracket init'
    --         fini
    --         body
    where
        -- init' = return ()
        -- fini = return ()
        -- body _ = do
        --     _ <-  (runStateT (iterM run dep) config)
        --     return ()

        run :: Deployment (StateT AMIConfig IO a) -> StateT AMIConfig IO a
        run (Container containerName next) = do
            lift $ T.putStrLn renderProvider
            liftIO $ putStrLn "some container cmd"
            next True
        run (Secret secretData next) = do
            liftIO $ putStrLn "some secret thingy"
            next ""
        run (Mount disk volume next) = do
            liftIO $ putStrLn "some storage mount"
            next True