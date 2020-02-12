{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Platform.DSL where

import "base"           Data.Semigroup ((<>))
import "base"           Data.Typeable (Typeable)
import "base"           Data.Data     (Data)
import "text"           Data.Text     (Text)
import "aeson"          Data.Aeson    (Value)
import "free"           Control.Monad.Free
import "free"           Control.Monad.Free.TH
import "platform-types" Platform.Types

-- ULTRA ULTRA simplified stub
-- Many many open questions here:
data PlatformCmd a
    = Container Text (ContainerID -> a)
    | Queue QueueID a
    | Produce ContainerID QueueID a
    | Consume ContainerID QueueID a
    | Failure a
    deriving (Typeable, Functor)

type Platform = Free PlatformCmd

makeFree ''PlatformCmd

--------------------
(|-->) :: ContainerID -> ContainerID -> Platform QueueID
(|-->) producerID consumerID = do
    let queueID = QueueID $ "direct-" <> (_containerID_name producerID) <> "-" <> (_containerID_name consumerID)
    queue queueID
    produce producerID queueID
    consume consumerID queueID
    return queueID

test :: Platform ()
test = do
    rest      <- container "atidot/webserver"
    jobrunner <- container "atidot/jobrunner"
    rest |--> jobrunner
    return ()

-- this is for the simple testing with platform-process
testPrototype :: Platform ()
testPrototype = do
    producer <- container "atidot/producer"
    consumer <- container "atidot/consumer"
    producer |--> consumer
    return ()
