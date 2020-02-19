{-# LANGUAGE PackageImports #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Platform.Process where

import           "base"                     Control.Monad.IO.Class (MonadIO, liftIO)
import           "base"                     Data.Maybe (fromJust)
import           "lens"                     Control.Lens
import           "data-default"             Data.Default (Default, def)
import           "text"                     Data.Text (Text, append, unpack, drop, strip)
import qualified "text"                     Data.Text as T
import           "mtl"                      Control.Monad.State.Class (MonadState, gets)
import           "mtl"                      Control.Monad.Writer.Lazy (MonadWriter, runWriterT)
import           "exceptions"               Control.Monad.Catch (MonadMask, bracket)
import           "free"                     Control.Monad.Free
import           "platform-types"           Platform.Types
import           "platform-dsl"             Platform.DSL
import           "shelly"                   Shelly
import           "platform-harness"         Platform.Harness.Types (HarnessScript(..))
import           "platform-packaging"       Platform.Packaging
import           "platform-packaging-types" Platform.Packaging.Types

-- TODO: The sequencing in `body` and in `run` has no error-detection.

-- TODO: move this def to another file
type ProcessConfig = ()

-- The separation of connectionActions and containerActions allows
-- the two to be sequenced. That is, all containerActions occur
-- before the connectionActions begin.
data ProcessState
    = ProcessState
        { _processState_containers :: ![ContainerID]
        , _processState_connections :: ![(ContainerID, ContainerID)]
        , _processState_containerActions :: !(Sh ())
        , _processState_connectionActions :: !(Sh ())
        }
makeLenses ''ProcessState

instance Default ProcessState where
    def = ProcessState []
                       []
                       (return ())
                       (return ())

-- currently, calls to runProcess call `dummyRun` when atidot/producer is launched,
-- and otherwise does nothing. I have this at a level of proof-of-concept to see
-- how to bring Docker, Shelly and Haskell together to launch Python apps.
runProcess :: (Monad m, MonadIO m, MonadWriter HarnessScript m, MonadState ProcessState m, MonadMask m)
           => ProcessConfig
           -> Platform a
           -> m a
runProcess config script
    = bracket init'
              fini
              body
    where
        init' = shelly . startDockerDefault $ "rabbitmq:latest"
        fini _ = return ()
        body amqpContainer = do
            result <- iterM run script
            return result

        run :: (Monad m, MonadIO m, MonadWriter HarnessScript m, MonadState ProcessState m, MonadMask m)
            => PlatformCmd (m a)
            -> m a
        run (Container name return') = do
            newContainer <- shelly $ ContainerID <$> startDockerLocal name
            return' newContainer

        run (Queue queueID return') = do
            undefined -- Declare all of the queues to the amqpContainer
            return'

        run (Produce _ _ return') = return'
        run (Consume _ _ return') = return'

-- These magic functions should be replaced by some image-selection logic
producerPath :: Text
producerPath = "/home/atidot/platform/testDockers/producer"

consumerPath :: Text
consumerPath = "/home/atidot/platform/testDockers/consumer"

launch :: Text -> Sh ()
launch "atidot/producer" = dummyLaunch -- startDockerLocal producerPath
launch "atidot/consumer" = pure () --startDockerLocal consumerPath
launch _ = undefined

connect :: Text
        -> Text
        -> Sh ()
connect "atidot/producer" "atidot/consumer" = pure ()
connect _ _ = undefined

docker :: Shelly.FilePath
docker = fromText "/usr/bin/docker"

launchRabbitServer :: Sh ()
launchRabbitServer = do
    run_ docker ["pull", "rabbitmq"]
    rabbitID <- run docker ["create", "rabbitmq"]
    run_ docker ["start", strip rabbitID]

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

startDockerDefault :: Text -> Sh Text
startDockerDefault image = do
    run_ docker ["pull", image]
    dockerID <- run docker ["create", image]
    startDocker [dockerID]
    return dockerID

startDockerLocal :: Text -> Sh Text
startDockerLocal image = do
    dockerIDRaw <- run docker ["build", "-q", image]
    let dockerID = getDockerID dockerIDRaw
    startDocker [dockerID]
    return dockerID

-- this should check for well-formed input
-- inputs look like "sha256:d0ca3dadf...."
getDockerID :: Text -> Text
getDockerID input = T.drop 7 input

newPath :: Text
newPath = "$PATH:/home/atidot/platform/static/testDockers/consumer:/home/atidot/platform/static/testDockers/producer"

platformSetup :: Sh Text
platformSetup = do
    setenv "PATH" newPath
    startDockerLocal "rabbitmq"
