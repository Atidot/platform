module Atidot.Platform.Terraform where

import "base"                   Control.Monad(void)
import "base"                   Data.Traversable(for)
import "base"                   Data.Monoid
import "lens"                   Control.Lens
import "terraform-hs"           Language.Terraform.Core
import "terraform-hs"           Language.Terraform.Aws

import qualified "containers"   Data.Map as M
import qualified "text"         Data.Text as T
import qualified "terraform-hs" Language.Terraform.Util.Text as T

simpleConfig :: T.Text -> TF ()
simpleConfig zone = do
  awsVpc <- awsVpc "atidot-vpc" "10.0.0.0/16" $ \vpcParams -> vpcParams{_vpc_enable_dns_support = True, _vpc_enable_dns_hostnames = True}
  awsInternetGateway' "atidot-env-gw" (vpc_id awsVpc)
  awsSn <- awsSubnet "atidot-subnet" (vpc_id awsVpc) "${cidrsubnet(aws_vpc.example_atidot-vpc.cidr_block, 3, 1)}" $ set sn_availability_zone (zone <> "a") -- <-- replace here after implementation
  awsRt <- awsRouteTable "atidot-rt" (vpc_id awsVpc) $ \rtParams -> rtParams{_rt_tags = ("cidr_block" =: "0.0.0.0/0") <> ("gateway_id" =: "${aws_internet_gateway.example_atidot-env-gw.id}")}  -- add route entry
  awsRouteTableAssociation' "atidot-env-assoc" (sn_id awsSn) (rt_id awsRt)
  awsSg <- awsSecurityGroup "atidot-sg" $ set sg_vpc_id (Just $ vpc_id awsVpc)
                               . set sg_ingress
                                  [ ingressOnPort 22
                                  ]
                               . set sg_egress
                                  [ egressAll
                                  ]

  inst <- awsInstance "atidot-instance" "ami-2757f631" "t2.micro" $ set i_vpc_security_group_ids [sg_id awsSg]
                                                                  . set i_subnet_id (Just $ sn_id awsSn)
  awsEip "atidot-env" $ \eipParams -> eipParams{_eip_vpc = True, _eip_instance = Just $ (i_id inst)}
  return ()


ingressOnPort :: Int -> IngressRuleParams
ingressOnPort port = IngressRuleParams
  { _ir_from_port = port
  , _ir_to_port = port
  , _ir_protocol = "tcp"
  , _ir_cidr_blocks = ["0.0.0.0/0"]
  }

egressAll :: EgressRuleParams
egressAll = EgressRuleParams
  { _er_from_port = 0
  , _er_to_port = 0
  , _er_protocol = "-1"
  , _er_cidr_blocks = ["0.0.0.0/0"]
  }

(=:) = M.singleton

-- to be added declaratively and replace the comment in line 20
cidrSubnet :: AwsId AwsSubnet -> Int -> Int -> String
cidrSubnet = undefined

mkDep :: IO ()
mkDep = generateFiles "" $ do
  let zone = "us-east-1"
  withNameScope "example" $ do
    newAws (makeAwsParams zone){aws_profile="default"}
    simpleConfig zone
