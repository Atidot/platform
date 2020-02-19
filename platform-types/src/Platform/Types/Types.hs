{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Platform.Types.Types where

import "base" Data.Data (Data)
import "base" Data.Typeable (Typeable)
import "base" GHC.Generics (Generic)
import "aeson" Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import "text" Data.Text (Text)

data ContainerID =
  ContainerID
    { _containerID_name :: !Text
    }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance ToJSON ContainerID where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ContainerID where

data QueueID =
  QueueID
    { _queueID_name :: !Text
    }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance ToJSON QueueID where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON QueueID where

data DatabaseID =
  DatabaseID
    { _databaseID_name :: !Text
    }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance ToJSON DatabaseID where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON DatabaseID where

data FileID =
  FileID
    { _fileID_name :: !Text
    }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance ToJSON FileID where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON FileID where
