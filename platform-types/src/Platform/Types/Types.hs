{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Platform.Types.Types where

import "base"    GHC.Generics (Generic)
import "base"    Data.Typeable (Typeable)
import "base"    Data.Data (Data)
import "text"    Data.Text (Text)

data ContainerID
    = ContainerID
    { _containerID_name :: !Text
    } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

data QueueID
    = QueueID
    { _queueID_name :: !Text
    } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

