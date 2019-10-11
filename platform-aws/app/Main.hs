{-# LANGUAGE PackageImports #-}
module Main where

import           "base"                    System.IO (stdin)
import qualified "bytestring"              Data.ByteString.Lazy.Char8 as B8 (putStrLn)
import           "data-default"            Data.Default (Default, def)
import           "optparse-applicative"    Options.Applicative
import           "mtl"                     Control.Monad.State (execStateT, evalStateT)
import           "stratosphere"            Stratosphere.Template (encodeTemplate)
import           "platform-types"          Platform.Types
import           "platform-dsl"            Platform.DSL
import qualified "platform-dsl"            Platform.DSL as DSL (test)
import           "platform-aws"            Platform.AWS


main :: IO ()
main = do
    newState <- flip execStateT def $ runAWS () DSL.test
    let template = _awsState_template newState
    B8.putStrLn $ encodeTemplate template
