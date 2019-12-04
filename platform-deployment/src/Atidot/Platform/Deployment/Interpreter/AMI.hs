{-# LANGUAGE QuasiQuotes #-}
module Atidot.Platform.Deployment.Interpreter.AMI where

import "text" Data.Text (Text)
import qualified "text" Data.Text as T
import qualified "text" Data.Text.IO as T
import "free"           Control.Monad.Free
import                  Atidot.Platform.Deployment
import "mtl"           Control.Monad.Writer (Writer)
import "mtl"            Control.Monad.State
import "mtl"            Control.Monad.Identity (Identity(..))

import "ginger"         Text.Ginger
import "raw-strings-qq" Text.RawString.QQ
import "exceptions"     Control.Monad.Catch (MonadMask, bracket)

data AMIConfig =
    AMIConfig

nullResolver :: IncludeResolver Identity
nullResolver = const $ return Nothing

toTemplate :: String -> Template SourcePos
toTemplate template = either (error . show) id . runIdentity $
    parseGinger nullResolver Nothing template

renderProvider :: T.Text
renderProvider = do
    let ctx :: GVal (Run SourcePos (Writer Text) Text)
        ctx = dict
            [ (("region" :: Text) ~> ("bobo" :: Text))
            , (("profile" :: Text) ~> (5 :: Int))
            , (("vpc-name" :: Text) ~> (undefined :: Text))
            , (("gateway-name" :: Text) ~> (undefined :: Text))
            , (("subnet-name" :: Text) ~> (undefined :: Text))
            , (("zone" :: Text) ~> ("east-us-1" :: Text))
            , (("route-table-name" :: Text) ~> (undefined :: Text))
            , (("route-table-assoc-name" :: Text) ~> (undefined :: Text))
            , (("security-group-name" :: Text) ~> (undefined :: Text))
            , (("instance-name" :: Text) ~> (undefined :: Text))
            , (("eip-name" :: Text) ~> (undefined :: Text))
            , (("key-name" :: Text) ~> (undefined :: Text))
            , (("key-public" :: Text) ~> (undefined :: Text))
            ]
    easyRender ctx $ toTemplate provider

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
  availability_zone = "{{zone}}a"
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
-- // to login
-- // $ ssh ubuntu@<public_dns> -i ~/.ssh/terraform-keys2



runAMI :: AMIConfig -> DeploymentM a -> IO ()
runAMI config dep = do
    _ <-  (runStateT (iterM run dep) config)
    return ()
    -- bracket init'
    --         fini
    --         body
    where
        -- init' = return ()
        -- fini = return ()
        -- body _ = do
        --     _ <-  (runStateT (iterM run dep) config)
        --     return ()

        run :: Deployment (StateT AMIConfig IO a) -> StateT AMIConfig IO a
        run (Container containerName next) = do
            lift $ T.putStrLn renderProvider
            liftIO $ putStrLn "some container cmd"
            next True
        run (Secret secretData next) = do
            liftIO $ putStrLn "some secret thingy"
            next ""
        run (Mount disk volume next) = do
            liftIO $ putStrLn "some storage mount"
            next True