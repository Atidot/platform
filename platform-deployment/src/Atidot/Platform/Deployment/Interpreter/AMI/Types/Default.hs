module Atidot.Platform.Deployment.Interpreter.AMI.Types.Default where
import "data-default" Data.Default
import Atidot.Platform.Deployment.Interpreter.AMI.Types.Types

instance Default AMIConfig where
    def = AMIConfig
        { _AMIConfig_region                = "us-east-1"
        , _AMIConfig_profile               = "default"
        , _AMIConfig_vpcName               = "atidot-vpc"
        , _AMIConfig_gatewayName           = "atidot-gw"
        , _AMIConfig_subnetName            = "atidot-subnet"
        , _AMIConfig_routeTableName        = "atidot-route-table"
        , _AMIConfig_routeTableAssocName   = "atidot-route-table-assoc"
        , _AMIConfig_securityGroupName     = "atidot-security-group"
        , _AMIConfig_instanceName          = "atidot-micro-instance"
        , _AMIConfig_eipName               = "atidot-eip"
        , _AMIConfig_keyName               = "terraform-keys2"
        , _AMIConfig_keyPublic             = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCStaX6KxA8ZjvbgNLOWI6eR26/+HkFjo1he91xyiuYcm6hGgObyaG3KkyJMoKHEEffVfFNSMFW/uiyz2rbWBzaavXCuoSj3KsdgC/fgatf3meC5Qfgq62ikDTjoY3cQaiUNM7R/dtR4h30AY1Dupq6AG7bDGnnyGaT3uAYpuswXUIFBJPef464FJRsZf4BGFIoVwRZE+2ATkL89GIMd3s/hM1JL6v7A8zqstYJDBWvhLZz23E1i6zNU7tAtt98qt5b+SDEJiCcXIUxb9FWMb0D11Lnbo/Aguz6gi/xs2RC2aLEJUBLpRcEGGxkp8AhmROOeGOXSLiPM3eorUJgmZHv talz@kubernetes-slave"
        }
