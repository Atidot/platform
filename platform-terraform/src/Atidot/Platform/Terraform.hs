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
  instId <- awsInstance' "atidot-instance" "ami-2757f631" "t2.micro"
  awsVpc <- awsVpc "atidot-vpc" "10.0.0.0/16" $ \vpcParams -> vpcParams{_vpc_enable_dns_support = True, _vpc_enable_dns_hostnames = True}
  awsEip "atidot-env" $ \eipParams -> eipParams{_eip_vpc = True, _eip_instance = Just $ (i_id instId)}
  awsInternetGateway' "atidot-env-gw" (vpc_id awsVpc)
  awsSn <- awsSubnet "atidot-subnet" (vpc_id awsVpc) "${cidrsubnet(aws_vpc.atidot-env.cidr_block, 3, 1)}" $ \sbParams -> sbParams{_sn_availability_zone = zone} -- <-- replace here after implementation
  awsRt <- awsRouteTable "atidot-rt" (vpc_id awsVpc) $ \rtParams -> rtParams{_rt_tags = ("cidr_block" =: "0.0.0.0/0") <> ("gateway_id" =: "${aws_internet_gateway.atidot-env-gw.id")}  -- add route entry
  awsRouteTableAssociation' "atidot-env-assoc" (sn_id awsSn) (rt_id awsRt)
  return ()

(=:) k v = M.singleton k v

-- to be added declaratively and replace the comment in line 20
cidrSubnet :: AwsId AwsSubnet -> Int -> Int -> String
cidrSubnet = undefined

mkDep :: IO ()
mkDep = generateFiles "" $
  let zone = "us-east-1"
  in withNameScope "example" $ do
    newAws (makeAwsParams zone){aws_profile="default"}
    simpleConfig zone
