{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Platform.Packaging.Pip.Types.Check (CheckOpts(..)) where

import           "base"         Data.Data (Data)
import           "base"         Data.Typeable (Typeable)
import           "base"         GHC.Generics (Generic)
import           "aeson"        Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import           "text"         Data.Text (Text)
import           "data-default" Data.Default (Default, def)
import Platform.Packaging.Pip.Types.Utils

newtype CheckOpts = CheckOpts ()
  deriving (Read, Eq, Ord, Bounded, Data, Typeable, Generic)

instance ToJSON CheckOpts where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON CheckOpts where

instance Default CheckOpts where
  def = CheckOpts ()

instance Opts CheckOpts where
  fmtOpts _ = []
