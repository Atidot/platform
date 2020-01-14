{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Platform.Packaging.Pip.Types.Search (SearchOpts(..)) where

import           "base"         Data.Data (Data)
import           "base"         Data.Typeable (Typeable)
import           "base"         GHC.Generics (Generic)
import           "aeson"        Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import           "text"         Data.Text (Text)
import           "data-default" Data.Default (Default, def)
import Platform.Packaging.Pip.Types.Utils

data SearchOpts
  = SearchOpts
      {_searchOpts_index :: !(Maybe URL)}
  deriving (Read, Eq, Ord, Data, Typeable, Generic)

instance ToJSON SearchOpts where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON SearchOpts where

instance Default SearchOpts where
  def = SearchOpts Nothing

instance Opts SearchOpts where
  fmtOpts (SearchOpts index) = noEmpty [txtPrint "index" index]
