module Atidot.Platform.Deployment.Interpreter.Utils where

import                        Prelude hiding (FilePath)
import           "turtle"     Turtle
import qualified "text"       Data.Text as T


terraformDepDir :: FilePath
terraformDepDir = "terraform_dep"

rSecretsDir :: Text
rSecretsDir = "/home/ubuntu/.secrets"

reduceShell :: Shell Line -> IO Text
reduceShell =  reduce $ Fold (<>) "" lineToText

getPublicDns = T.takeWhile (/= '"')
             . T.tail . T.dropWhile (/= '"')
             . snd
             . T.breakOn "public_dns"

getInstanceId = T.takeWhile (/= '"')
              . T.tail
              . T.dropWhile (/= '"')
              . snd
              . T.breakOn "id"
              . T.takeWhile (/= '}')
              . T.dropWhile (/= '{')
              . snd
              . T.breakOn "aws_instance"