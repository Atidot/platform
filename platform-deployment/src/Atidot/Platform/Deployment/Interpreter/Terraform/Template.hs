{-# LANGUAGE QuasiQuotes #-}
module Atidot.Platform.Deployment.Interpreter.Terraform.Template where

import "text"           Data.Text (Text)
import "ginger"         Text.Ginger
import "raw-strings-qq" Text.RawString.QQ
import "mtl"            Control.Monad.Writer (Writer)
import "mtl"            Control.Monad.Identity (Identity(..))
import "data-default" Data.Default
import Atidot.Platform.Deployment.Interpreter.AMI.Types hiding (DiskName,SecretName,VolumeName)
import Atidot.Platform.Deployment.Interpreter.AMI.Template hiding (awsInstance,allTemplates, awsEbsVolume)

instance Default TerraformExtendedConfig where
    def = TerraformExtendedConfig [] [] [] def $ zip ["xvdh","sdf","sdg","sdh","sdj"] ["vol-01ac704e80ba48949"]

type Cmd = String
type SecretName = String
type DiskName = String
type VolumeName = String
type Name = String
type DeviceName = String


data TerraformExtendedConfig = TerraformExtendedConfig
    { _TerraformExtendedConfig_instanceExec :: [Cmd]
    , _TerraformExtendedConfig_disks :: [(DeviceName,VolumeName)] -- start, stop, -- modify-stab
    , _TerraformExtendedConfig_secrets :: [SecretName]
    , _TerraformExtendedConfig_terraformConfig :: TerraformConfig
    , _TerraformExtendedConfig_availableDisks :: [(DeviceName,VolumeName)]
    }

renderTerraform :: TerraformExtendedConfig -> Text
renderTerraform (TerraformExtendedConfig cmds disks _secrets tconf _) =
    let awsInstanceTemplate = renderProvider tconf $ nullRemoteProvsioner cmds []
        otherTemplates = renderProvider tconf $ defTemplates
        (devNames, diskNames) = unzip disks
        ebsVolumes = foldl1 (<>) $ zipWith3 awsEbsVolume (map (\i -> "atidot_ebs_vol_" ++ show i) [1..]) devNames diskNames
    in otherTemplates <> ebsVolumes <> awsInstanceTemplate


defTemplates :: String
defTemplates = foldl1 (<>)
    [ provider
    , awsVpc
    , awsInternetGateway
    , awsSubnet
    , awsRouteTable
    , awsRouteTableAssoc
    , awsSecurityGroup
    , awsEip
    , awsKeyPair
    , awsInstance
    ]


awsInstance :: String
awsInstance = [r|
resource "aws_instance" "{{instanceName}}" {
  ami = "ami-2757f631"
  instance_type = "t2.micro"
  subnet_id = aws_subnet.{{subnetName}}.id
  key_name = "{{keyName}}"
  vpc_security_group_ids = [
    aws_security_group.{{securityGroupName}}.id
  ]
}
    |]

nullRemoteProvsioner :: [Cmd] -> [SecretName] -> String
nullRemoteProvsioner cmds _ = [r|
resource "null_resource" "example_provisioner" {

  triggers = {
    public_ip = aws_eip.{{eipName}}.id
    volume_id = aws_volume_attachment.atidot_ebs_vol_1.id
  }

  connection {
    user        = "ubuntu"
    host        =  aws_eip.{{eipName}}.public_ip
    agent       = false
    private_key = file("~/.ssh/{{keyName}}")
  }
    |] <>
    genExec cmds
  <> [r|
}
    |]
    where

        genExec :: [Cmd] -> String
        genExec cmds = [r|
  provisioner "remote-exec" {
    inline = [
|] <> unlines ( map ((\l -> l <> ","). show) cmds) <>
            [r|    ]
  }
            |]


awsEbsVolume :: Name -> DeviceName -> VolumeName -> Text
awsEbsVolume name deviceName volumeName = do
    let ctx :: GVal (Run SourcePos (Writer Text) Text)
        ctx = dict $ map (\(a,b) -> a ~> b)
            [ ("name"              , name              )
            , ("deviceName"        , deviceName        )
            , ("volumeName"        , volumeName        )
            ]
    easyRender ctx $ toTemplate template
        where
            template :: String
            template = [r|
resource "aws_volume_attachment" "{{name}}" {
  device_name = "/dev/{{deviceName}}"
  volume_id   = "{{volumeName}}"
  instance_id = aws_instance.atidot-micro-instance.id
  skip_destroy = true
}
  |]
