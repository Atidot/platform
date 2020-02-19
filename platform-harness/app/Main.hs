{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           "base"                    GHC.Generics (Generic)
import           "base"                    Data.Maybe (fromMaybe)
import           "base"                    Data.Function ((&))
import qualified "bytestring"              Data.ByteString.Lazy.Char8 as B8 (pack)
import           "data-default"            Data.Default (def)
import           "aeson"                   Data.Aeson (decode)
import           "text"                    Data.Text as T (Text, pack)
import           "mtl"                     Control.Monad.Reader (runReaderT)
import           "optparse-generic"        Options.Generic (getRecord, ParseRecord)
import           "platform-types"          Platform.Types
import           "platform-dsl"            Platform.DSL
import                                     Platform.Harness
import                                     Platform.Harness.Types

data CLI
    = CLI
    { inputscript :: String
    , hostcontainer   :: String
    , scriptopts  :: Maybe String
    } deriving (Generic, Show)

instance ParseRecord CLI where

main :: IO ()
main = getRecord "Harness" >>= \record -> do
    let script    = fromMaybe mempty $      B8.pack (inputscript record) &   decode :: HarnessScript
        opts      = fromMaybe def    $ fmap B8.pack (scriptopts  record) >>= decode :: HarnessConfig
    runReaderT
        (runHarness opts script)
        (HarnessState (T.pack . hostcontainer $ record) defaultURL)

defaultURL :: T.Text
defaultURL = "127.0.0.1"
