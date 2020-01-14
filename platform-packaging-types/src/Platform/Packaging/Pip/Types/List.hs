{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Platform.Packaging.Pip.Types.List (ListOpts(..)) where

import           "base"         Data.Data (Data)
import           "base"         Data.Typeable (Typeable)
import           "base"         GHC.Generics (Generic)
import           "aeson"        Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import           "text"         Data.Text (Text)
import           "data-default" Data.Default (Default, def)
import Platform.Packaging.Pip.Types.Utils

data ListOpts
  = ListOpts
      { _listOpts_outdated :: !(Maybe Bool),
        _listOpts_uptodate :: !(Maybe Bool),
        _listOpts_editable :: !(Maybe Bool),
        _listOpts_local :: !(Maybe Bool),
        _listOpts_user :: !(Maybe Bool),
        _listOpts_path :: !(Maybe FilePathT),
        _listOpts_pre :: !(Maybe Bool),
        _listOpts_format :: !(Maybe OutputFormat),
        _listOpts_notRequired :: !(Maybe Bool),
        _listOpts_excludeEditable :: !(Maybe Bool),
        _listOpts_includeEditable :: !(Maybe Bool),
        _listOpts_index :: !(Maybe URL),
        _listOpts_extraIndex :: !(Maybe URL),
        _listOpts_noIndex :: !(Maybe Bool),
        _listOpts_findLinks :: !(Maybe URL)
      }
  deriving (Read, Eq, Ord, Data, Typeable, Generic)

instance ToJSON ListOpts where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ListOpts where

instance Default ListOpts where
  def =
    ListOpts
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
      Nothing

instance Opts ListOpts where
  fmtOpts
    ( ListOpts
        outdated
        uptodate
        editable
        local
        user
        path
        pre
        format
        notRequired
        excludeEditable
        includeEditable
        index
        extraIndex
        noIndex
        findLinks
      ) =
      noEmpty
        [ boolPrint "outdated" outdated,
          boolPrint "uptodate" uptodate,
          boolPrint "editable" editable,
          boolPrint "local" local,
          boolPrint "user" user,
          txtPrint "path" path,
          boolPrint "pre" pre,
          formatPrint "format" format,
          boolPrint "not-required" notRequired,
          boolPrint "exclude-editable" excludeEditable,
          boolPrint "include-editable" includeEditable,
          txtPrint "index-url" index,
          txtPrint "extra-index-url" extraIndex,
          boolPrint "no-index" noIndex,
          txtPrint "find-links" findLinks
        ]
