{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Platform.Packaging.Pip.Types.Debug (DebugOpts(..)) where

import           "base"         Data.Data (Data)
import           "base"         Data.Typeable (Typeable)
import           "base"         GHC.Generics (Generic)
import           "aeson"        Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import           "text"         Data.Text (Text)
import           "data-default" Data.Default (Default, def)
import Platform.Packaging.Pip.Types.Utils

data DebugOpts
  = DebugOpts
      { _debugOpts_platform :: !(Maybe Text),
        _debugOpts_pythonVersion :: !(Maybe Text),
        _debugOpts_implementation :: !(Maybe Text),
        _debugOpts_abi :: !(Maybe Text)
      }
  deriving (Read, Show, Eq, Ord, Data, Typeable, Generic)

instance ToJSON DebugOpts where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON DebugOpts where

instance Default DebugOpts where
  def = DebugOpts Nothing Nothing Nothing Nothing

instance Opts DebugOpts where
  fmtOpts (DebugOpts plat vers impl abi) =
    noEmpty
      [ txtPrint "platform" plat,
        txtPrint "python-version" vers,
        txtPrint "implementation" impl,
        txtPrint "abi" abi
      ]
