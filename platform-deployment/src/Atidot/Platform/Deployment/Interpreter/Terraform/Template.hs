{-# LANGUAGE QuasiQuotes #-}
module Atidot.Platform.Deployment.Interpreter.Terraform.Template where

import "text"           Data.Text (Text)
import "ginger"         Text.Ginger
import "raw-strings-qq" Text.RawString.QQ
import "mtl"            Control.Monad.Writer (Writer)
import "mtl"            Control.Monad.Identity (Identity(..))
import "data-default" Data.Default
import Atidot.Platform.Deployment.Interpreter.AMI.Types
import Atidot.Platform.Deployment.Interpreter.AMI.Template hiding (toTemplate,renderProvider, awsInstance,allTemplates)

instance Default TerraformExtendedConfig where
    def = TerraformExtendedConfig [] [] [] def


data TerraformExtendedConfig = TerraformExtendedConfig
    { _TerraformExtendedConfig_instanceExec :: [String]
    , _TerraformExtendedConfig_disks :: [(String,String)] -- start, stop, -- modify-stab
    , _TerraformExtendedConfig_secrets :: [String]
    , _TerraformExtendedConfig_terraformConfig :: TerraformConfig
    }


toTemplate :: String -> Template SourcePos
toTemplate template = either (error . show) id . runIdentity $
    parseGinger nullResolver Nothing template

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
                , ("routeTableAssocName"    , _TerraformConfig_routeTableAssocName )
                , ("securityGroupName"      , _TerraformConfig_securityGroupName   )
                , ("instanceName"           , _TerraformConfig_instanceName        )
                , ("eipName"                , _TerraformConfig_eipName             )
                , ("keyName"                , _TerraformConfig_keyName             )
                , ("keyPublic"              , _TerraformConfig_keyPublic           )
                , ("s3BucketName"           , _TerraformConfig_s3BucketName        )
                ]

allTemplates :: String
allTemplates = foldl1 (<>)
    [ provider
    , awsVpc
    , awsInternetGateway
    , awsSubnet
    , awsRouteTable
    , awsRouteTableAssoc
    , awsSecurityGroup
    , awsInstance
    , awsEip
    , awsKeyPair
    , awsS3Bucket
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

awsEbsVolume :: String
awsEbsVolume = [r|
resource "aws_volume_attachment" "ebs_att" {
  device_name = "/dev/sdg"
  volume_id   = "vol-0d2df0f8a79885bc3"
  instance_id = aws_instance.atidot-micro-instance.id
  skip_destroy = true
}
  |]
