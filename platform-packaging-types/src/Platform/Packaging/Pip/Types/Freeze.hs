{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Platform.Packaging.Pip.Types.Freeze (FreezeOpts(..)) where

import           "base"         Data.Data (Data)
import           "base"         Data.Typeable (Typeable)
import           "base"         GHC.Generics (Generic)
import           "aeson"        Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import           "text"         Data.Text (Text)
import           "data-default" Data.Default (Default, def)
import Platform.Packaging.Pip.Types.Utils

data FreezeOpts
  = FreezeOpts
      { _freezeOpts_requirements :: !(Maybe FilePathT),
        _freezeOpts_findLinks :: !(Maybe URL),
        _freezeOpts_local :: !(Maybe Bool),
        _freezeOpts_user :: !(Maybe Bool),
        _freezeOpts_path :: !(Maybe FilePathT),
        _freezeOpts_all :: !(Maybe Bool),
        _freezeOpts_excludeEditable :: !(Maybe Bool)
      }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance ToJSON FreezeOpts where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON FreezeOpts where

instance Default FreezeOpts where
  def =
    FreezeOpts 
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing

instance Opts FreezeOpts where
  fmtOpts
    ( FreezeOpts
        requirements
        findLinks
        local
        user
        path
        all
        excludeEditable
      ) =
      noEmpty
        [ txtPrint "requirements" requirements,
          txtPrint "find-links" findLinks,
          boolPrint "local" local,
          boolPrint "user" user,
          txtPrint "path" path,
          boolPrint "all" all,
          boolPrint "exclude-editable" excludeEditable
        ]
