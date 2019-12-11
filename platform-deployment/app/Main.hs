module Main where

import "data-default" Data.Default
import                Atidot.Platform.Deployment
import                Atidot.Platform.Deployment.Interpreter.AMI
import                Atidot.Platform.Deployment.Interpreter.AMI.Types

main :: IO ()
main = runAMI def hello
