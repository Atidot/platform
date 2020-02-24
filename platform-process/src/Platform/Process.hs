{-# LANGUAGE PackageImports #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
module Platform.Process where

import           "base"                     Control.Monad.IO.Class (MonadIO, liftIO)
import           "base"                     Data.Maybe (fromJust)
import           "lens"                     Control.Lens
import           "aeson"                    Data.Aeson (encode)
import           "bytestring"               Data.ByteString.Lazy.Char8 as B8 (unpack)
import           "data-default"             Data.Default (Default, def)
import           "text"                     Data.Text (Text, append, strip)
import qualified "text"                     Data.Text as T (pack, drop)
import           "mtl"                      Control.Monad.State.Class (MonadState, gets)
import           "mtl"                      Control.Monad.Writer.Lazy (MonadWriter, runWriterT)
import           "exceptions"               Control.Monad.Catch (MonadMask, bracket)
import           "free"                     Control.Monad.Free
import           "shelly"                   Shelly (Sh, shelly, fromText, setenv, unlessM, run_)
import qualified "shelly"                   Shelly as Sh (run, FilePath)
import           "platform-types"           Platform.Types
import           "platform-dsl"             Platform.DSL
import           "platform-harness"         Platform.Harness.Types (HarnessScript(..))
import           "platform-packaging"       Platform.Packaging
import           "platform-packaging-types" Platform.Packaging.Types

-- TODO: The sequencing in `body` and in `run` has no error-detection.
-- TODO: Rename "Config" and "/etc/platform/" literals
-- TODO: move this def to another file
type ProcessConfig = ()

-- The separation of connectionActions and containerActions allows
-- the two to be sequenced. That is, all containerActions occur
-- before the connectionActions begin.
data ProcessState = ProcessState { _processState_containers :: ![ContainerID] }
makeLenses ''ProcessState

instance Default ProcessState where
    def = ProcessState []

-- currently, calls to runProcess call `dummyRun` when atidot/producer is launched,
-- and otherwise does nothing. I have this at a level of proof-of-concept to see
-- how to bring Docker, Shelly and Haskell together to launch Python apps.
runProcess :: (Monad m, MonadIO m, MonadState ProcessState m, MonadMask m)
           => ProcessConfig
           -> Platform a
           -> m a
runProcess config script
    = bracket init'
              fini
              body
    where
        init' = shelly $ startDockerDefault "rabbitmq:latest" "Config" "/etc/platform/"
        fini _ = return ()
        body amqpContainer = do
            (result, harnessScript) <- runWriterT $ iterM run script
            unlessM ((== 0) . length <$> gets _processState_containers)
                    (shellyCopyConfig harnessScript amqpContainer)
            return result

        shellyCopyConfig harnessScript amqpContainer = do
            firstContainer <- head <$> gets _processState_containers
            shelly $ writeAMQPLocation  firstContainer =<< getContainerURL amqpContainer
            shelly $ writeHarnessScript firstContainer harnessScript

        run :: (Monad m, MonadIO m, MonadWriter HarnessScript m, MonadState ProcessState m, MonadMask m)
            => PlatformCmd (m a)
            -> m a
        run (Container name return') = do
            newContainer <- shelly $ startDockerLocal name "Config" "/etc/platform/"
            processState_containers <>= [newContainer]
            return' newContainer
        run (Queue   _   return') = return'
        run (Produce _ _ return') = return'
        run (Consume _ _ return') = return'

-- These magic functions should be replaced by some image-selection logic
producerPath :: Text
producerPath = "/home/atidot/platform/testDockers/producer"

consumerPath :: Text
consumerPath = "/home/atidot/platform/testDockers/consumer"

writeHarnessScript :: ContainerID
                   -> HarnessScript
                   -> Sh ()
writeHarnessScript c hs = copyToDocker harnessScript c "/etc/platform/harnessScript.json"
    where harnessScript = T.pack . B8.unpack . encode $ hs

writeAMQPLocation :: ContainerID
                  -> Text
                  -> Sh ()
writeAMQPLocation c loc = copyToDocker loc c "/etc/platform/amqpserver"

copyToDocker :: Text        -- ^ File contents
             -> ContainerID -- ^ Dockerfile to write to
             -> Text        -- ^ Filepath to copy to
             -> Sh ()       -- ^ The resulting shell action
copyToDocker s (ContainerID name) fp = do
    run_ "printf" [s, ">", "./tempfile"]
    run_ docker ["cp", "./tempfile", name <> ":" <> fp]
    run_ "rm" ["./tempfile"]

docker :: Sh.FilePath
docker = fromText "/usr/bin/docker"

-- switch from run_ (which suppresses stdout) to run (which returns stdout)
-- this shelly code is not currently error-safe
startDocker :: [Text] -> Sh ContainerID
startDocker args = do
    ContainerID <$> Sh.run docker (["run"] ++ args)

startDockerWithVolume :: Text           -- ^ Image ID
                      -> Text           -- ^ Volume name
                      -> Text           -- ^ Volume mount location
                      -> Sh ContainerID -- ^ Resulting container ID
startDockerWithVolume image volume mount
  = startDocker ["-q", "-v", volume <> ":" <> mount, image]

startDockerDefault :: Text           -- ^ Image name
                   -> Text           -- ^ Volume name
                   -> Text           -- ^ Volume mount location
                   -> Sh ContainerID -- ^ Resulting container ID
startDockerDefault image vol mount = do
    run_ docker ["pull", image]
    imageID <- Sh.run docker ["create", "-q", image]
    startDockerWithVolume imageID vol mount

startDockerLocal :: Text           -- ^ Dockerfile location
                 -> Text           -- ^ Volume name
                 -> Text           -- ^ Volume mount location
                 -> Sh ContainerID -- ^ Resulting container ID
startDockerLocal dLoc vol mount = do
    imageIDRaw <- Sh.run docker ["build", "-q", dLoc]
    let imageID = getDockerID imageIDRaw
    startDockerWithVolume imageID vol mount

-- this should check for well-formed input
-- inputs look like "sha256:d0ca3dadf...."
getDockerID :: Text -> Text
getDockerID input = T.drop 7 input

newPath :: Text
newPath = "$PATH:/home/atidot/platform/static/testDockers/consumer:/home/atidot/platform/static/testDockers/producer"

getContainerURL :: ContainerID -> Sh Text
getContainerURL (ContainerID id')
  = Sh.run docker
           [ "inspect"
           , "-f"
           , "'{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}'"
           , id']
