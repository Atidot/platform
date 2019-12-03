module Main where

import Atidot.Platform.Deployment
import Atidot.Platform.Deployment.Interpreter.Test

main :: IO ()
main = runTest TestConfig kiss
