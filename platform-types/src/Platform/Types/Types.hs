{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Platform.Types.Types where

import "base" Data.Data (Data)
import "text" Data.Text (Text)
import "base" Data.Typeable (Typeable)
import "base" GHC.Generics (Generic)


data Secret
    = Secret
    { _secret_x :: !Int
    }
    deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

data File
    = File
    { _file_x :: !Int
    }
    deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)


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
