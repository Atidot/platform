{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           "base"                    Debug.Trace (trace)
import           "base"                    GHC.Generics (Generic)
import           "base"                    Data.Maybe (fromMaybe, maybe, fromJust)
import           "base"                    Data.Function ((&))
import qualified "bytestring"              Data.ByteString.Lazy.Char8 as B8 (ByteString, pack, unpack, readFile)
import           "data-default"            Data.Default (def)
import           "aeson"                   Data.Aeson (decode, FromJSON)
import           "text"                    Data.Text as T (Text, pack)
import           "exceptions"              Control.Monad.Catch (Exception, throwM)
import           "mtl"                     Control.Monad.Reader (runReaderT)
import           "optparse-generic"        Options.Generic (getRecord, ParseRecord)
import           "directory"               System.Directory (doesFileExist)
import           "extra"                   System.Time.Extra (sleep)
import           "shelly"                  Shelly hiding (FilePath, sleep, command, trace)
import           "platform-types"          Platform.Types
import           "platform-dsl"            Platform.DSL
import                                     Platform.Harness
import                                     Platform.Harness.Types

data CLI
    = CLI
    { entrypoint     :: String
    , entrypointArgs :: [String]
    , amqpurl        :: Maybe String
    , inputscript    :: Maybe String
    , scriptopts     :: Maybe String
    } deriving (Generic, Show)

instance ParseRecord CLI where

data HarnessException
    = NoHostnameException
    deriving (Show)

instance Exception HarnessException

main :: IO ()
main = getRecord "Harness" >>= \record -> do
    let opts = fromMaybe def                  $ maybeDecode scriptopts  record ::    HarnessConfig
    script  <- maybe     waitForScript return $ maybeDecode inputscript record :: IO HarnessScript
    amqpURL <- maybe     waitForAMQP   return $             amqpurl     record :: IO String
    trace "read from stdin" $ return ()
    hostContainer <- whichContainerAmI
    trace "figured out the container I am" $ return ()
    runReaderT
        (runHarness opts script)
        (HarnessState hostContainer $ T.pack amqpURL)
    trace "made it past runReaderT" $ return ()
    log <- shelly
         . run (fromText . T.pack . entrypoint $ record)
         . map T.pack
         $ entrypointArgs record
    trace "executed the command" $ return ()
    shelly $ writefile (fromText "./log") log
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
whichContainerAmI = do
    name <- shelly $ get_env "HOSTNAME"
    maybe (throwM NoHostnameException) return name
