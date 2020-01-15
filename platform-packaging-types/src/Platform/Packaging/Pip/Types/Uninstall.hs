{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Platform.Packaging.Pip.Types.Uninstall (UninstallInput(..), UninstallOpts(..)) where

import           "base"         Data.Data (Data)
import           "base"         Data.Typeable (Typeable)
import           "base"         GHC.Generics (Generic)
import           "aeson"        Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import           "text"         Data.Text (Text)
import           "data-default" Data.Default (Default, def)
import Platform.Packaging.Pip.Types.Utils
import Platform.Packaging.Pip.Types.Input

data UninstallInput
  = UnInstPkgs PkgList
  | UnInstFiles FileList
  deriving (Read, Show, Eq, Ord, Data, Typeable, Generic)

instance ToJSON UninstallInput where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON UninstallInput where

instance Input UninstallInput where
    fmtInput (UnInstPkgs pkgs) = pkgs
    fmtInput (UnInstFiles files) = files

data UninstallOpts
  = UninstallOpts
      { _uninstallOpts_requirement :: !(Maybe FilePathT),
        _uninstallOpts_yes :: !(Maybe Bool)
      }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance ToJSON UninstallOpts where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON UninstallOpts where

instance Default UninstallOpts where
  def = UninstallOpts Nothing Nothing

instance Opts UninstallOpts where
  fmtOpts (UninstallOpts requirement yes) =
    noEmpty [txtPrint "requirement" requirement, boolPrint "yes" yes]
