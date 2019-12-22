{-# LANGUAGE QuasiQuotes #-}
module Atidot.Platform.Deployment.Interpreter.Terraform.Template where

import "text"           Data.Text (Text)
import "ginger"         Text.Ginger
import "raw-strings-qq" Text.RawString.QQ
import "mtl"            Control.Monad.Writer (Writer)
import "mtl"            Control.Monad.Identity (Identity(..))
import "data-default" Data.Default
import Atidot.Platform.Deployment.Interpreter.AMI.Types hiding (DiskName,SecretName,VolumeName)
import Atidot.Platform.Deployment.Interpreter.AMI.Template hiding (renderProvider, awsInstance,allTemplates, awsEbsVolume)

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
    let awsInstanceTemplate = renderProvider tconf $ awsInstance cmds disks []
        otherTemplates = renderProvider tconf $ defTemplates
        (devNames, diskNames) = unzip disks
        ebsVolumes = foldl1 (<>) $ zipWith3 awsEbsVolume (map (\i -> "atidot_ebs_vol_" ++ show i) [1..]) devNames diskNames
    in otherTemplates <> ebsVolumes <> awsInstanceTemplate


renderProvider :: TerraformConfig -> String -> Text
renderProvider config template = do
    let ctx :: GVal (Run SourcePos (Writer Text) Text)
        ctx = toCtx config
    easyRender ctx $ toTemplate template
        where
            toCtx :: TerraformConfig -> GVal (Run SourcePos (Writer Text) Text)
            toCtx conf = dict $ map (\(a,b) -> a ~> b conf)
                [ ("region"                 , _TerraformConfig_region              )
                , ("profile"                , _TerraformConfig_profile             )
                , ("vpcName"                , _TerraformConfig_vpcName             )
                , ("gatewayName"            , _TerraformConfig_gatewayName         )
                , ("subnetName"             , _TerraformConfig_subnetName          )
                , ("routeTableName"         , _TerraformConfig_routeTableName      )
                , ("routeTableAssocName"   , _TerraformConfig_routeTableAssocName )
                , ("securityGroupName"      , _TerraformConfig_securityGroupName   )
                , ("instanceName"           , _TerraformConfig_instanceName        )
                , ("eipName"                , _TerraformConfig_eipName             )
                , ("keyName"                , _TerraformConfig_keyName             )
                , ("keyPublic"              , _TerraformConfig_keyPublic           )
                , ("s3BucketName"           , _TerraformConfig_s3BucketName        )
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
    ]


awsInstance :: [Cmd] -> [(DiskName,VolumeName)] -> [SecretName] -> String
awsInstance cmds _ _ = [r|
resource "aws_instance" "{{instanceName}}" {
  ami = "ami-2757f631"
  instance_type = "t2.micro"
  subnet_id = aws_subnet.{{subnetName}}.id
  key_name = "{{keyName}}"
  vpc_security_group_ids = [
    aws_security_group.{{securityGroupName}}.id
  ]
    |] <>
    genExec cmds
  <> [r|
}
    |]
    where
        genExec :: [Cmd] -> String
        genExec = unlines . map genExecSingle
        genExecSingle :: Cmd -> String
        genExecSingle cmd = [r|
  provisioner "local-exec" {
    command = |] <> show cmd <>
            [r|
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
