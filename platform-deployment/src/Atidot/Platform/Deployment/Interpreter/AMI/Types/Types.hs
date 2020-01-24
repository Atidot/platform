{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Atidot.Platform.Deployment.Interpreter.AMI.Types.Types where

import "base" Data.Data (Data)
import "base" Data.Typeable (Typeable)
import "base" GHC.Generics (Generic)
import "aeson" Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import "text" Data.Text (Text)
import "uuid" Data.UUID

type ContainerName = Text
type DiskName = Text
type VolumeName = Text
type SecretName = UUID
type SecretData = String
type SecretAsMount = FilePath -- for the case that the secret is a file

data AMIConfig =
    AMIConfig
    { _AMIConfig_secrets                    :: [(SecretName,(SecretData,Maybe SecretAsMount,Maybe ContainerName))]
    , _AMIConfig_configs                    :: [(String,Maybe String)]
    , _AMIConfig_mounts                     :: [(VolumeName,(DiskName,Maybe ContainerName))]
    , _AMIConfig_terraformConfig            :: TerraformConfig
    }
    deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance ToJSON AMIConfig where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON AMIConfig

data TerraformConfig =
    TerraformConfig
    { _TerraformConfig_region                :: !Text
    , _TerraformConfig_profile               :: !Text
    , _TerraformConfig_vpcName               :: !Text
    , _TerraformConfig_gatewayName           :: !Text
    , _TerraformConfig_subnetName            :: !Text
    , _TerraformConfig_routeTableName        :: !Text
    , _TerraformConfig_routeTableAssocName   :: !Text
    , _TerraformConfig_securityGroupName     :: !Text
    , _TerraformConfig_instanceName          :: !Text
    , _TerraformConfig_eipName               :: !Text
    , _TerraformConfig_keyName               :: !Text
    , _TerraformConfig_s3BucketName          :: !Text
    , _TerraformConfig_ebs_volume            :: !Text
    }
    deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance ToJSON TerraformConfig where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON TerraformConfig
