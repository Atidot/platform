module Atidot.Platform.Test where

-- import Control.Monad.State
import "free"     Control.Monad.Free
-- import "free" Control.Monad.Free.TH
-- import Data.Map (Map)
-- import qualified Data.Map as M
import            Atidot.Platform.Deployment


runTest :: TestConfig -> DeploymentM a -> IO ()
runTest _ dep = do
    _ <- iterM run dep
    return ()
    where
        run :: Deployment (IO a) -> IO a
        run _ = do
            putStrLn "hello"
            return undefined
