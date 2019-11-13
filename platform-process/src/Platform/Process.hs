{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Platform.Process where

import "base" Control.Monad.IO.Class (MonadIO, liftIO)
import "base" Data.Maybe (fromJust)
import "lens" Control.Lens
import "data-default" Data.Default (Default, def)
import "text" Data.Text (Text, append, unpack, drop, strip)
import "mtl" Control.Monad.State.Class (MonadState, gets)
import "exceptions" Control.Monad.Catch (MonadMask, bracket)
import "free" Control.Monad.Free
import "platform-types" Platform.Types
import "platform-dsl" Platform.DSL
import "shelly" Shelly

-- TODO: The sequencing in `body` and in `run` has no error-detection.

-- TODO: move this def to another file
type ProcessConfig = ()

-- The separation of connectionActions and containerActions allows
-- the two to be sequenced. That is, all containerActions occur
-- before the connectionActions begin. 
data ProcessState
    = ProcessState
        { _processState_containers :: [ContainerID]
        , _processState_connections :: [(ContainerID, ContainerID)]
        , _processState_containerActions :: Sh ()
        , _processState_connectionActions :: Sh ()
        , _processState_actions :: Sh ()
        } 
makeLenses ''ProcessState

instance Default ProcessState where
    def = ProcessState []
                       []
                       (pure ())
                       (pure ())
                       platformSetup

-- currently, calls to runProcess call `dummyRun` when atidot/producer is launched,
-- and otherwise does nothing. I have this at a level of proof-of-concept to see
-- how to bring Docker, Shelly and Haskell together to launch Python apps.
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
            -- TODO: change things so that containers are given a harness
            -- based on the connections they are a part of
            result <- iterM run script
            containerActions <- gets _processState_containerActions
            connectionActions <- gets _processState_connectionActions
            processState_actions .= (containerActions >> connectionActions)
            
            return result
            

        run :: (Monad m, MonadState ProcessState m, MonadMask m)
            => PlatformCmd (m a)
            -> m a
        run (Container name return') = do
            as <- gets _processState_containerActions
            processState_containerActions .= (as >> launch name)
            let newContainer = ContainerID name
            return' newContainer

        run (Connection (ContainerID name1)
                        (ContainerID name2)
                        return'
            ) = do
            as <- gets _processState_connectionActions
            processState_connectionActions .= (as >> connect name1 name2)
            return'

-- These magic functions should be replaced by some image-selection logic
producerPath = "/home/atidot/platform/testDockers/producer"
consumerPath = "/home/atidot/platform/testDockers/consumer"
launch "atidot/producer" = dummyLaunch -- startDockerLocal producerPath
launch "atidot/consumer" = pure () --startDockerLocal consumerPath
launch _ = undefined
connect :: Text -> Text -> Sh ()
connect "atidot/producer" "atidot/consumer" = pure ()
connect _ _ = undefined

docker :: Shelly.FilePath
docker = fromText "/usr/bin/docker"

dummyLaunch :: Sh ()
dummyLaunch = do
    run_ docker ["pull", "rabbitmq"]
    rabbitID <- run docker ["create", "rabbitmq"]
    run_ docker ["start", strip rabbitID]
    producerImage <- run docker ["build", "-q", producerPath]
    producerID <- run docker ["create", strip producerImage]
    run_ docker ["start", strip producerID]
    consumerImage <- run docker ["build", "-q", consumerPath]
    consumerID <- run docker ["create", strip consumerImage]
    run_ docker ["start", strip consumerID]
    

-- TODO: expand this to actually do something
-- switch from run_ (which suppresses stdout) to run (which returns stdout)
-- this shelly code is not currently error-safe
startDocker :: [Text] -> Sh ()
startDocker args = do
    run_ docker (["start"] ++ args)

startDockerDefault :: Text -> Sh ()
startDockerDefault image = do
    run_ docker ["pull", image] 
    dockerID <- run docker ["create", image]
    startDocker [dockerID]

startDockerLocal :: Text -> Sh ()
startDockerLocal image = do
    dockerIDRaw <- run docker ["build", "-q", image]
    startDocker [getDockerID dockerIDRaw]

-- this should check for well-formed input
-- inputs look like "sha256:d0ca3dadf...."
getDockerID :: Text -> Text
getDockerID input = Data.Text.drop 7 input
newPath :: Text
newPath = "$PATH:/home/atidot/platform/static/testDockers/consumer:/home/atidot/platform/static/testDockers/producer"
platformSetup :: Sh ()
platformSetup = do
    setenv "PATH" newPath
    startDockerLocal "rabbitmq"
