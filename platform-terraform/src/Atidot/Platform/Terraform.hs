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

simpleConfig :: TF ()
simpleConfig = do
  inst <- awsInstance' "atidot-instance" "ami-2757f631" "t2.micro"
  awsVpc "atidot-vpc" "10.0.0.0/16" $ \vpcParams -> vpcParams{_vpc_enable_dns_support = True, _vpc_enable_dns_hostnames = True}
  awsEip "atidot-ip" $ \eipParams -> eipParams{_eip_vpc = True, _eip_instance = Just $ (i_id inst)}
  return ()

mkDep :: IO ()
mkDep = generateFiles "" $
  withNameScope "example" $ do
    newAws (makeAwsParams "us-east-1"){aws_profile="default"}
    simpleConfig
