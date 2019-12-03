module Main where

import Atidot.Platform.Deployment
import Atidot.Platform.Test

main :: IO ()
main = runTest TestConfig kiss
