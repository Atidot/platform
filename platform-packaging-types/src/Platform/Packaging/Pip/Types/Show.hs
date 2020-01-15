{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Platform.Packaging.Pip.Types.Show (ShowOpts(..))
where

import           "base"         Data.Data (Data)
import           "base"         Data.Typeable (Typeable)
import           "base"         GHC.Generics (Generic)
import           "aeson"        Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import           "text"         Data.Text (Text)
import           "data-default" Data.Default (Default, def)
import Platform.Packaging.Pip.Types.Utils

data ShowOpts
  = ShowOpts
      {_showOpts_files :: !(Maybe Bool)}
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance ToJSON ShowOpts where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ShowOpts where

instance Default ShowOpts where
  def = ShowOpts Nothing

instance Opts ShowOpts where
  fmtOpts (ShowOpts files) = noEmpty [boolPrint "files" files]
