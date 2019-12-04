{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Atidot.Platform.Deployment.Interpreter.AMI.Types.Types where

import "base" Data.Data (Data)
import "base" Data.Typeable (Typeable)
import "base" GHC.Generics (Generic)
import "text" Data.Text (Text)


data AMIConfig =
    AMIConfig
    { _AMIConfig_region                :: !Text
    , _AMIConfig_profile               :: !Text
    , _AMIConfig_vpcName               :: !Text
    , _AMIConfig_gatewayName           :: !Text
    , _AMIConfig_subnetName            :: !Text
    , _AMIConfig_routeTableName        :: !Text
    , _AMIConfig_routeTableAssocName   :: !Text
    , _AMIConfig_securityGroupName     :: !Text
    , _AMIConfig_instanceName          :: !Text
    , _AMIConfig_eipName               :: !Text
    , _AMIConfig_keyName               :: !Text
    , _AMIConfig_keyPublic             :: !Text
    }
    deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)
