{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Platform.Process where

import "base" Control.Monad.IO.Class (MonadIO, liftIO)
import "base" Data.Maybe (fromJust)
import "lens" Control.Lens
import "data-default" Data.Default (Default, def)
import "mtl" Control.Monad.State.Class (MonadState, gets)
import "exceptions" Control.Monad.Catch (MonadMask, bracket)
import "free" Control.Monad.Free
import "platform-types" Platform.Types
import "platform-dsl" Platform.DSL
import "executor" Executor

-- TODO: move this def to another file
type ProcessConfig = ()

data ProcessState
    = ProcessState
        { _processState_containers :: [ContainerID]
        --, _processState_connections :: [Connection] 
        } deriving (Show, Read, Eq)
makeLenses ''ProcessState

runProcess :: (Monad m, MonadState ProcessState m, MonadMask m)
           => ProcessConfig
           -> Platform a
           -> m a
runProcess config script
    = bracket init'
              fini
              body
    where
        init' = return ()
        fini _ = return ()
        body _ = do
            result <- iterM run script
            return result

        run :: (Monad m, MonadState ProcessState m, MonadMask m)
            => PlatformCmd (m a)
            -> m a
        run cmd = return undefined
