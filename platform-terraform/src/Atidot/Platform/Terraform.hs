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

simpleConfig :: TF AwsInstance
simpleConfig = awsInstance' "example" "ami-2757f631" "t2.micro"

mkDep :: IO ()
mkDep = generateFiles "" $ do
  withNameScope "example" $ do
    newAws (makeAwsParams "us-east-1"){aws_profile="default"}
    simpleConfig
  return ()
