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
    , _TerraformExtendedConfig_disks :: [(DeviceName,VolumeName)]
    , _TerraformExtendedConfig_secrets :: [SecretName]
    , _TerraformExtendedConfig_terraformConfig :: TerraformConfig
    , _TerraformExtendedConfig_availableDisks :: [(DeviceName,VolumeName)]
    }

renderTerraform :: TerraformExtendedConfig -> Text
renderTerraform (TerraformExtendedConfig cmds disks secrets tconf _) =
    let awsInstanceTemplate = renderProvider tconf $ nullRemoteProvsioner cmds []
        otherTemplates = renderProvider tconf $ defTemplates
        (devNames, diskNames) = unzip disks
        ebsVolumes = foldl1 (<>) $ zipWith3 awsEbsVolume (map (\i -> "atidot_ebs_vol_" ++ show i) [1..]) devNames diskNames
        secretsProvsioning = renderProvider tconf $ secretsProvisioner secrets
    in foldl1 (<>) $
      [ otherTemplates
      , ebsVolumes
      , awsInstanceTemplate
      , secretsProvsioning
      ]


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
    , envProvisioner
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


envProvisioner :: String
envProvisioner = [r|
resource "null_resource" "env_setter" {

  triggers = {
    public_ip = aws_eip.{{eipName}}.id
    volume_id = aws_volume_attachment.{{ebsVolumeName}}.id
  }

  connection {
    user        = "ubuntu"
    host        = aws_eip.{{eipName}}.public_ip
    agent       = false
    private_key = file("~/.ssh/{{keyName}}")
  }

  provisioner "remote-exec" {
    inline = [
"sudo apt update",
"sudo apt install docker.io -y",
"sudo usermod -aG docker $USER",
"sudo apt install jq -y",
"sudo apt install python3-pip -y",
"pip3 install awscli --upgrade --user",
"mkdir -p ~/.aws",
"printf \"[default]\naws_access_key_id = ${var.aws_access_key_id}\naws_secret_access_key = ${var.aws_secret_access_key}\n\" >> ~/.aws/credentials",
"printf \"[default]\nregion = ${var.aws_default_region}\noutput = ${var.aws_default_format}\n\" >> ~/.aws/config",
]
  }

}
    |]

secretsProvisioner :: [SecretName] -> String
secretsProvisioner [] = []
secretsProvisioner secrets = [r|
resource "null_resource" "secrets_provisioner" {

  triggers = {
    public_ip = aws_eip.{{eipName}}.id
    volume_id = aws_volume_attachment.{{ebsVolumeName}}.id
    env_setter = null_resource.env_setter.id
    mount_and_pull = null_resource.mount_and_pull.id

  }

  connection {
    user        = "ubuntu"
    host        = aws_eip.{{eipName}}.public_ip
    agent       = false
    private_key = file("~/.ssh/{{keyName}}")
  }

  provisioner "remote-exec" {
    inline = [|] <>
    unlines (map commandifySecret secrets)
    <> [r|]
  }

}
    |]
    where
      commandifySecret :: String -> String
      commandifySecret secret = [r|
"printf \"export SECRET=\" >> ~/.bashrc",
"aws secretsmanager get-secret-value --secret-id |] <> secret <> [r| | jq '.SecretString'  >> ~/.bashrc",
"printf \"\n\">> ~/.bashrc",
|]


nullRemoteProvsioner :: [Cmd] -> [SecretName] -> String
nullRemoteProvsioner cmds _ = [r|
resource "null_resource" "mount_and_pull" {

  triggers = {
    public_ip = aws_eip.{{eipName}}.id
    volume_id = aws_volume_attachment.{{ebsVolumeName}}.id
    env_setter = null_resource.env_setter.id

  }

  connection {
    user        = "ubuntu"
    host        = aws_eip.{{eipName}}.public_ip
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


-- later, add the values from files
-- this part is used to config the aws cli to read from secrets

awsConfigVars :: Text
awsConfigVars = [r|
variable "aws_access_key_id" {
  type = string
}

variable "aws_secret_access_key" {
  type = string
}

variable "aws_default_region" {
  type = string
  default = "us-east-1"
}

variable "aws_default_format" {
  type = string
  default = "json"
}
    |]