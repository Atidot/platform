{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           "base"                    GHC.Generics (Generic)
import           "base"                    Data.Maybe (fromMaybe, maybe, fromJust)
import           "base"                    Data.Function ((&))
import qualified "bytestring"              Data.ByteString.Lazy.Char8 as B8 (ByteString, pack, unpack, readFile)
import           "data-default"            Data.Default (def)
import           "aeson"                   Data.Aeson (decode, FromJSON)
import           "text"                    Data.Text as T (Text, pack)
import           "mtl"                     Control.Monad.Reader (runReaderT)
import           "optparse-generic"        Options.Generic (getRecord, ParseRecord)
import           "directory"               System.Directory (doesFileExist)
import           "extra"                   System.Time.Extra (sleep)
import           "shelly"                  Shelly hiding (FilePath, sleep)
import           "platform-types"          Platform.Types
import           "platform-dsl"            Platform.DSL
import                                     Platform.Harness
import                                     Platform.Harness.Types

data CLI
    = CLI
    { entrypoint     :: String
    , entrypointArgs :: [String]
    , amqpurl        :: String
    , inputscript    :: Maybe String
    , scriptopts     :: Maybe String
    } deriving (Generic, Show)

instance ParseRecord CLI where

main :: IO ()
main = getRecord "Harness" >>= \record -> do
    let opts = fromMaybe def                  $ maybeDecode scriptopts  record ::    HarnessConfig
    script  <- maybe     waitForScript return $ maybeDecode inputscript record :: IO HarnessScript
    hostContainer <- whichContainerAmI
    runReaderT
        (runHarness opts script)
        (HarnessState hostContainer (T.pack . amqpurl $ record)) -- TODO: take the ampqurl and the hostcontainer
                                                                 -- clearly
    shelly . run_ (fromText . T.pack . entrypoint $ record)
           . map T.pack
           $ entrypointArgs record
    where
        maybeDecode :: FromJSON b
                    => (CLI -> Maybe String)
                    -> CLI
                    -> Maybe b
        maybeDecode accessor record = fmap B8.pack (accessor record) >>= decode

waitForScript :: IO HarnessScript
waitForScript = fromJust . decode <$> waitForFile harnessScriptLocation

waitForAMQP :: IO String
waitForAMQP = B8.unpack <$> waitForFile amqpLocation

waitForFile :: FilePath -> IO B8.ByteString
waitForFile location = doesFileExist location >>= \case
                 False -> sleep 1 >> waitForFile location
                 True  -> B8.readFile location

harnessScriptLocation :: FilePath
harnessScriptLocation = "/etc/platform/harnessScript.json"

amqpLocation :: FilePath
amqpLocation = "/etc/platform/amqpserver"

whichContainerAmI :: IO Text
whichContainerAmI = undefined
