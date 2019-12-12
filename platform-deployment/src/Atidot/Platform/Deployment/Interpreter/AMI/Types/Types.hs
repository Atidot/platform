{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Atidot.Platform.Deployment.Interpreter.AMI.Types.Types where

import "base" Data.Data (Data)
import "base" Data.Typeable (Typeable)
import "base" GHC.Generics (Generic)
import "text" Data.Text (Text)


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
    , _TerraformConfig_keyPublic             :: !Text
    }
    deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)
