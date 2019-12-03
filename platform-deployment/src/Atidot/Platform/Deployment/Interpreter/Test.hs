module Atidot.Platform.Test where

-- import Control.Monad.State
import "free"     Control.Monad.Free
-- import "free" Control.Monad.Free.TH
-- import Data.Map (Map)
-- import qualified Data.Map as M
import            Atidot.Platform.Deployment
import "mtl"      Control.Monad.State

data TestConfig =
    TestConfig


runTest :: TestConfig -> DeploymentM a -> IO ()
runTest config dep = do
    _ <-  (runStateT (iterM run dep) config)
    return ()
    where
        run :: Deployment (StateT TestConfig IO a) -> StateT TestConfig IO a
        run (Container containerName next) = do
            liftIO $ putStrLn "some container cmd"
            next True
        run (Secret secretData next) = do
            liftIO $ putStrLn "some secret thingy"
            next ""
        run (Mount disk volume next) = do
            liftIO $ putStrLn "some storage mount"
            next True
