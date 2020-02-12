{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Platform.AWS where

import "base"           Control.Monad.IO.Class (MonadIO, liftIO)
import "lens"           Control.Lens
import "text"           Data.Text (Text)
import "data-default"   Data.Default (Default, def)
import "mtl"            Control.Monad.State.Class (MonadState, gets)
import "exceptions"     Control.Monad.Catch (MonadMask, bracket)
import "free"           Control.Monad.Free
import "stratosphere"   Stratosphere
import "stratosphere"   Stratosphere.Template
import "stratosphere"   Stratosphere.Resources
import "stratosphere"   Stratosphere.ResourceProperties
import "stratosphere"   Stratosphere.Resources.ECSTaskDefinition
import "platform-types" Platform.Types
import "platform-dsl"   Platform.DSL

---------
-- TODO: move to another file
type AWSConfig = ()

data AWSState
    = AWSState
    { _awsState_template :: !Template
    , _awsState_rabbitmqClient :: !(Maybe ContainerID)
    } deriving (Show, Eq)
makeLenses ''AWSState

instance Default AWSState where
    def = AWSState (template $ Resources []) Nothing

---------
runAWS :: (Monad m, MonadState AWSState m, MonadMask m)
          => AWSConfig
          -> Platform a
          -> m a
runAWS config script
    = bracket init'
              fini
              body
    where
        init'  = do
            rabbitID <- run (Container "rabbitmq" (return))
            assign awsState_rabbitmqClient (Just rabbitID)
            return rabbitID

        fini _ = return ()
        body _ = do
            result <- iterM run script
            return result

        run :: (Monad m, MonadState AWSState m, MonadMask m)
             => PlatformCmd (m a)
             -> m a
        run (Container name return') = do
            -- new container definition in ECS
            let newECS = ecsTaskDefinition
                       & ecstdContainerDefinitions ?~ [ ecsTaskDefinitionContainerDefinition (Literal name)
                                                                                             (Literal name)
                                                      ]
            let newResource = resource "TODO" newECS

            -- updat state
            awsState_template . templateResources <>= Resources [newResource]

            -- return new ContainerID
            let newContainer = ContainerID name
            return' newContainer

        run (Queue (QueueID name)
                   return'
            ) = do
            return'

        run (Produce (ContainerID name1)
                     (QueueID name2)
                      return'
            ) = do
            return'

        run (Consume (ContainerID name1)
                     (QueueID name2)
                      return'
            ) = do
            return'
