{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Platform.Harness where

import "base" Data.Functor ((<&>))
import "base" Data.Semigroup ((<>))
import "base" Control.Monad (when)
import "base" Control.Monad.IO.Class (MonadIO)
import "text" Data.Text (Text, append, splitOn)
import "extra" Control.Monad.Extra (whenM)
import "mtl" Control.Monad.Reader (MonadReader, ask)
import "exceptions" Control.Monad.Catch (MonadMask, bracket)
import "free" Control.Monad.Free
import "turtle" Turtle (Shell)
import "turtle" Turtle.Prelude (need, export)
import "platform-types" Platform.Types
import "platform-dsl" Platform.DSL

newtype MessengerProfile = MessengerProfile { _messengerProfile_channelName :: Text }
data HarnessState
    = HarnessState
    { _harnessState_containerName :: Text
    , _harnessState_amqpURL :: Text
    } deriving (Show)
type HarnessConfig = ()
-- The type of each Messenger is measured from the perspective of a particular
-- container. I.e. if A --> B --> C are one-way channels, then according to A, B is a Consumer
-- but according to C, B is a Producer.
data Messenger
    = Consumer MessengerProfile
    | Producer MessengerProfile

type Messengers = [Messenger]

setMessengerVar :: MonadIO m
                => Messenger
                -> m ()
setMessengerVar (Consumer cProfile)
    = export "PLATFORM_CONSUMERS" $ "$PLATFORM_CONSUMERS:" <> _messengerProfile_channelName cProfile
setMessengerVar (Producer pProfile)
    = export "PLATFORM_PRODUCERS" $ "$PLATFORM_PRODUCERS:" <> _messengerProfile_channelName pProfile

getEnv :: MonadIO m => m Messengers
getEnv = do
    c <- need "PLATFORM_CONSUMERS"
    p <- need "PLATFORM_PRODUCERS"
    let listC = maybe mempty (makeProfiles Consumer) c
    let listP = maybe mempty (makeProfiles Producer) p
    return $ listC <> listP
    where
        makeProfiles f = map (f . MessengerProfile) . splitOn ":"

runHarness :: (MonadIO m, MonadReader HarnessState m, MonadMask m)
           => HarnessConfig
           -> Platform a
           -> m a
runHarness config script
    = bracket init'
              fini
              body
    where
        init' = return ()
        fini _ = return ()
        body _ = do
            result <- iterM run script
            export "PLATFORM_AMQP_URL" =<< _harnessState_amqpURL <$> ask
            export "PLATFORM_HARNESS" "1" -- Set this last to help indicate partial execution
            return result

        run :: (MonadIO m, MonadReader HarnessState m, MonadMask m)
            => PlatformCmd (m a)
            -> m a
        run (Container name return') = do
            return' $ ContainerID name
        run (Produce (ContainerID origin)
                     (QueueID dest)
                     return'
            ) = do
            myName <- ask <&> _harnessState_containerName
            when (origin == myName)
                 (produceFor dest)
            return'
        run (Consume (ContainerID dest)
                     (QueueID origin)
                     return'
            ) = do
            myName <- ask <&> _harnessState_containerName
            when (dest == myName)
                 (consumeFrom origin)
            return'

-- If the harness is consuming from X, then X is considered a producer.
consumeFrom :: MonadIO m
            => Text
            -> m ()
consumeFrom = setMessengerVar . Producer . MessengerProfile

-- If the harness is producing for X, then X is considered a consumer.
produceFor :: MonadIO m
           => Text
           -> m ()
produceFor = setMessengerVar . Consumer . MessengerProfile
