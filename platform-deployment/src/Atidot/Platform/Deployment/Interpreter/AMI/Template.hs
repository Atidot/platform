{-# LANGUAGE QuasiQuotes #-}
module Atidot.Platform.Deployment.Interpreter.AMI.Template where

import "text"           Data.Text (Text)
import "ginger"         Text.Ginger
import "raw-strings-qq" Text.RawString.QQ
import "mtl"            Control.Monad.Writer (Writer)
import "mtl"            Control.Monad.Identity (Identity(..))
import Atidot.Platform.Deployment.Interpreter.AMI.Types

nullResolver :: IncludeResolver Identity
nullResolver = const $ return Nothing

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
                , ("s3BucketName"           , _TerraformConfig_s3BucketName        )
                , ("ebsVolumeName"          , _TerraformConfig_ebs_volume          )
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
    ]

provider :: String
provider = [r|
provider "aws" {
    version = "~> 2.39"
    region = "{{region}}"
    profile = "{{profile}}"
}
    |]

awsVpc :: String
awsVpc = [r|
resource "aws_vpc" "{{vpcName}}" {
  cidr_block = "10.0.0.0/16"
  enable_dns_hostnames = true
}
    |]

awsInternetGateway :: String
awsInternetGateway = [r|
resource "aws_internet_gateway" "{{gatewayName}}" {
  vpc_id = aws_vpc.{{vpcName}}.id
}
    |]

awsSubnet :: String
awsSubnet = [r|
resource "aws_subnet" "{{subnetName}}" {
  vpc_id = aws_vpc.{{vpcName}}.id
  cidr_block = cidrsubnet(aws_vpc.{{vpcName}}.cidr_block, 3, 1)
  availability_zone = "{{region}}a"
}
    |]

awsRouteTable :: String
awsRouteTable = [r|
resource "aws_route_table" "{{routeTableName}}" {
  vpc_id = aws_vpc.{{vpcName}}.id
  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.{{gatewayName}}.id
  }
}
    |]

awsRouteTableAssoc :: String
awsRouteTableAssoc = [r|
resource "aws_route_table_association" "{{routeTableAssocName}}" {
  subnet_id = aws_subnet.{{subnetName}}.id
  route_table_id = aws_route_table.{{routeTableName}}.id
}
    |]

awsSecurityGroup :: String
awsSecurityGroup = [r|
resource "aws_security_group" "{{securityGroupName}}" {
  ingress {
    from_port = "22"
    to_port = "22"
    protocol = "tcp"
    cidr_blocks = [
      "0.0.0.0/0"
    ]
  }
  egress {
    from_port = "0"
    to_port = "0"
    protocol = "-1"
    cidr_blocks = [
      "0.0.0.0/0"
    ]
  }
  vpc_id = aws_vpc.{{vpcName}}.id
}
    |]

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

awsEip :: String
awsEip = [r|
resource "aws_eip" "{{eipName}}" {
  vpc = "true"
  instance = aws_instance.{{instanceName}}.id
}
    |]

awsKeyPair :: String
awsKeyPair = [r|
resource "aws_key_pair" "{{keyName}}" {
  key_name   = "{{keyName}}"
  public_key = file("~/.ssh/{{keyName}}.pub")
}
    |]

awsS3Bucket :: String
awsS3Bucket = [r|
resource "aws_s3_bucket" "{{s3BucketName}}" {
  bucket = "atidot-tf-test-bucket"
  acl    = "private"
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
