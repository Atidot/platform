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

renderProvider :: AMIConfig -> String -> Text
renderProvider amiConfig template = do
    let ctx :: GVal (Run SourcePos (Writer Text) Text)
        ctx = toCtx amiConfig
    easyRender ctx $ toTemplate template
        where
            toCtx :: AMIConfig -> GVal (Run SourcePos (Writer Text) Text)
            toCtx conf = dict $ map (\(a,b) -> a ~> b conf)
                [ ("region"                 , _AMIConfig_region              )
                , ("profile"                , _AMIConfig_profile             )
                , ("vpc-name"               , _AMIConfig_vpcName             )
                , ("gateway-name"           , _AMIConfig_gatewayName         )
                , ("subnet-name"            , _AMIConfig_subnetName          )
                , ("route-table-name"       , _AMIConfig_routeTableName      )
                , ("route-table-assoc-name" , _AMIConfig_routeTableAssocName )
                , ("security-group-name"    , _AMIConfig_securityGroupName   )
                , ("instance-name"          , _AMIConfig_instanceName        )
                , ("eip-name"               , _AMIConfig_eipName             )
                , ("key-name"               , _AMIConfig_keyName             )
                , ("key-public"             , _AMIConfig_keyPublic           )
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
    , awsKeyPair
    ]

provider :: String
provider = [r|
provider "aws" {
    region = "{{region}}"
    profile = "{{profile}}"
}
    |]

awsVpc :: String
awsVpc = [r|
resource "aws_vpc" "{{vpc-name}}" {
  cidr_block = "10.0.0.0/16"
  enable_dns_hostnames = "true"
}
    |]

awsInternetGateway :: String
awsInternetGateway = [r|
resource "aws_internet_gateway" "{{gateway-name}}" {
  vpc_id = "${aws_vpc.{{vpc-name}}.id}"
}
    |]

awsSubnet :: String
awsSubnet = [r|
resource "aws_subnet" "{{subnet-name}}" {
  vpc_id = "${aws_vpc.{{vpc-name}}.id}"
  cidr_block = "${cidrsubnet(aws_vpc.{{vpc-name}}.cidr_block, 3, 1)}"
  availability_zone = "{{region}}a"
}
    |]

awsRouteTable :: String
awsRouteTable = [r|
resource "aws_route_table" "{{route-table-name}}" {
  vpc_id = "${aws_vpc.{{vpc-name}}.id}"
  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = "${aws_internet_gateway.{{gateway-name}}.id}"
  }
}
    |]

awsRouteTableAssoc :: String
awsRouteTableAssoc = [r|
resource "aws_route_table_association" "{{route-table-assoc-name}}" {
  subnet_id = "${aws_subnet.{{subnet-name}}.id}"
  route_table_id = "${aws_route_table.{{route-table-name}}.id}"
}
    |]

awsSecurityGroup :: String
awsSecurityGroup = [r|
resource "aws_security_group" "{{security-group-name}}" {
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
  vpc_id = "${aws_vpc.{{vpc-name}}.id}"
}
    |]

awsInstance :: String
awsInstance = [r|
resource "aws_instance" "{{instance-name}}" {
  ami = "ami-2757f631"
  instance_type = "t2.micro"
  subnet_id = "${aws_subnet.{{subnet-name}}.id}"
  key_name = "terraform-keys2"
  vpc_security_group_ids = [
    "${aws_security_group.{{security-group-name}}.id}"
  ]
}
    |]

awsEip :: String
awsEip = [r|
resource "aws_eip" "{{eip-name}}" {
  vpc = "true"
  instance = "${aws_instance.{{instance-name}}.id}"
}
    |]

awsKeyPair :: String
awsKeyPair = [r|
resource "aws_key_pair" "{{key-name}}" {
  key_name   = "{{key-name}}"
  public_key = "{{key-public}}"
}
    |]