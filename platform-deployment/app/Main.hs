module Main where

import Atidot.Platform.Deployment
import Atidot.Platform.Deployment.Interpreter.AMI
import Atidot.Platform.Deployment.Interpreter.AMI.Types
import "data-default" Data.Default

main :: IO ()
main = runAMI def kiss
