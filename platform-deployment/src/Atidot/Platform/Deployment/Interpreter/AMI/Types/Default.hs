{-# OPTIONS_GHC -fno-warn-orphans #-}
module Atidot.Platform.Deployment.Interpreter.AMI.Types.Default where

import "data-default" Data.Default
import Atidot.Platform.Deployment.Interpreter.AMI.Types.Types

instance Default AMIConfig where
    def = AMIConfig [] [] [] def

instance Default TerraformConfig where
    def = TerraformConfig
        { _TerraformConfig_region                = "us-east-1"
        , _TerraformConfig_profile               = "default"
        , _TerraformConfig_vpcName               = "atidot-vpc"
        , _TerraformConfig_gatewayName           = "atidot-gw"
        , _TerraformConfig_subnetName            = "atidot-subnet"
        , _TerraformConfig_routeTableName        = "atidot-route-table"
        , _TerraformConfig_routeTableAssocName   = "atidot-route-table-assoc"
        , _TerraformConfig_securityGroupName     = "atidot-security-group"
        , _TerraformConfig_instanceName          = "atidot-micro-instance"
        , _TerraformConfig_eipName               = "atidot-eip"
        , _TerraformConfig_keyName               = "terraform-keys2"
        , _TerraformConfig_s3BucketName          = "atidot-s3-bucket"
        , _TerraformConfig_ebs_volume            = "atidot_ebs_vol_1"
        }
