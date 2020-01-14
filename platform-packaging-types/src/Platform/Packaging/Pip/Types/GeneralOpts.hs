{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Platform.Packaging.Pip.Types.GeneralOpts (GeneralOpts(..)) where

import "base"         Data.Data (Data)
import "base"         Data.Typeable (Typeable)
import "base"         GHC.Generics (Generic)
import "aeson"        Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import "data-default" Data.Default (Default, def)
import "text"         Data.Text (Text)
import Platform.Packaging.Pip.Types.Utils

data GeneralOpts
  = GeneralOpts
      { _generalOpts_help :: !(Maybe Bool)
      , _generalOpts_isolated :: !(Maybe Bool)
      , _generalOpts_verbose :: !(Maybe Bool)
      , _generalOpts_version :: !(Maybe Bool)
      , _generalOpts_quiet :: !(Maybe Bool)
      , _generalOpts_log :: !(Maybe FilePathT)
      , _generalOpts_proxy :: !(Maybe Text)
      , _generalOpts_retries :: !(Maybe Int)
      , _generalOpts_timeout :: !(Maybe Int)
      , _generalOpts_existsAction :: !(Maybe Action)
      , _generalOpts_trustedHost :: !(Maybe URL)
      , _generalOpts_cert :: !(Maybe FilePathT)
      , _generalOpts_clientCert :: !(Maybe FilePathT)
      , _generalOpts_cacheDir :: !(Maybe FilePathT)
      , _generalOpts_noCacheDir :: !(Maybe Bool)
      , _generalOpts_disablePipVersionCheck :: !(Maybe Bool)
      , _generalOpts_noColor :: !(Maybe Bool)
      }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance ToJSON GeneralOpts where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON GeneralOpts where

instance Default GeneralOpts where
    def = GeneralOpts Nothing
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                      (Just True) -- Disables pip version check warning messages
                      Nothing

instance Opts GeneralOpts where
    fmtOpts (GeneralOpts help
                         isolated
                         verbose
                         version
                         quiet
                         log
                         proxy
                         retries
                         timeout
                         existsAction
                         trustedHost
                         cert
                         clientCert
                         cacheDir
                         noCacheDir
                         disablePipVersionCheck
                         noColor)
                           = noEmpty [ boolPrint "help" help
                                     , boolPrint "isolated" isolated
                                     , boolPrint "verbose" verbose
                                     , boolPrint "version" version
                                     , boolPrint "quiet" quiet
                                     , txtPrint "log" log
                                     , txtPrint "proxy" proxy
                                     , intPrint "retries" retries
                                     , intPrint "timeout" timeout
                                     , actionPrint "exists-action" existsAction
                                     , txtPrint "trusted-host" trustedHost
                                     , txtPrint "cert" cert
                                     , txtPrint "client-cert" clientCert
                                     , txtPrint "cache-dir" cacheDir
                                     , boolPrint "no-cache-dir" noCacheDir
                                     , boolPrint "disable-pip-version-check" disablePipVersionCheck
                                     , boolPrint "no-color" noColor
                                     ]
