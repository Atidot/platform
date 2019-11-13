{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           "base"                    System.IO (stdin)
import qualified "bytestring"              Data.ByteString.Lazy.Char8 as B (ByteString, readFile, putStrLn, hGetContents, unpack)
import           "aeson"                   Data.Aeson (encode)
import           "optparse-applicative"    Options.Applicative
import           "data-default"            Data.Default (def)
import           "mtl"                     Control.Monad.State (execStateT)
import           "platform-types"          Platform.Types
import           "platform-dsl"            Platform.DSL as DSL (test, testPrototype)
import           "platform-process"        Platform.Process
import           "shelly"                  Shelly

main :: IO ()
main = do
    newState <- flip execStateT def $ runProcess () DSL.testPrototype
    let actions = _processState_actions newState
    shelly actions
    return ()
