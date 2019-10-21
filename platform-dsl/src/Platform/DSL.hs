{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Platform.DSL where

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
    | Connection ContainerID ContainerID a
    | Failure a
    deriving (Typeable, Functor)

type Platform = Free PlatformCmd

makeFree ''PlatformCmd

--------------------
(|-->) :: PlatformCmd a -> PlatformCmd a -> PlatformCmd a
(|-->) c@(Container t f) d@(Container t' f') = Connection c d ()
(|-->) c d = Failure ()

test :: Platform ()
test = do
    rest      <- Container "atidot/webserver" (const ())
    jobrunner <- Container "atidot/jobrunner" (const ())
    rest |--> jobrunner
