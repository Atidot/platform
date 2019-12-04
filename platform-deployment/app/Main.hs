module Main where

import Atidot.Platform.Deployment
import Atidot.Platform.Deployment.Interpreter.AMI

main :: IO ()
main = runAMI AMIConfig kiss
