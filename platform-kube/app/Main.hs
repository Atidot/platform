{-# LANGUAGE PackageImports #-}
module Main where

import           "base"                    System.IO (stdin)
import qualified "bytestring"              Data.ByteString.Lazy.Char8 as B (ByteString, readFile, putStrLn, hGetContents, unpack)
import           "aeson"                   Data.Aeson (encode)
import           "optparse-applicative"    Options.Applicative
import           "platform-types"          Platform.Types
import           "platform-dsl"            Platform.DSL
import           "platform-kube"           Platform.Kube

main :: IO ()
main = do
    return ()
