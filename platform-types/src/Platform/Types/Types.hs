{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Platform.Types.Types where

import "base" Data.Data (Data)
import "text" Data.Text (Text)
import "base" Data.Typeable (Typeable)
import "base" GHC.Generics (Generic)

data ContainerID =
  ContainerID
    { _containerID_name :: !Text
    }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

data QueueID =
  QueueID
    { _queueID_name :: !Text
    }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

data DatabaseID =
  DatabaseID
    { _databaseID_name :: !Text
    }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

data FileID =
  FileID
    { _fileID_name :: !Text
    }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

data Container =
  Container
    { _container_id :: !ContainerID
    }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

data Queue =
  Queue
    { _queue_id :: !QueueID
    , _queue_origin :: !Container
    , _queue_destination :: !Container
    }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

data Database =
  Database
    { _database_container :: !Container
    }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

data File =
  File
    { _file_id :: !FileID
    }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)
