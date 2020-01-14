{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Platform.Packaging.Pip.Types.Config (ConfigInput (..), ConfigOpts(..)) where

import           "base"         Data.Data (Data)
import           "base"         Data.Typeable (Typeable)
import           "base"         GHC.Generics (Generic)
import           "aeson"        Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import           "text"         Data.Text (Text)
import           "data-default" Data.Default (Default, def)
import Platform.Packaging.Pip.Types.Utils

data ConfigInput
  = List
  | Edit
  | Get !Text
  | Set !Text !Text
  | Unset !Text
  deriving (Read, Show, Eq, Ord, Data, Typeable, Generic)

instance ToJSON ConfigInput where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ConfigInput where

instance Input ConfigInput where
    fmtInput List = ["list"]
    fmtInput Edit = ["edit"]
    fmtInput (Get name) = ["get", name]
    fmtInput (Set name value) = ["set", name, value]
    fmtInput (Unset name) = ["unset", name]

data ConfigOpts
  = ConfigOpts
      { _configOpts_editor :: !(Maybe Text),
        _configOpts_global :: !(Maybe Bool),
        _configOpts_user :: !(Maybe Bool),
        _configOpts_site :: !(Maybe Bool)
      }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance ToJSON ConfigOpts where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ConfigOpts where

instance Default ConfigOpts where
  def =
    ConfigOpts
      Nothing
      Nothing
      Nothing
      Nothing

instance Opts ConfigOpts where
  fmtOpts
    ( ConfigOpts
        editor
        global
        user
        site
      ) =
      noEmpty
        [ txtPrint "editor" editor,
          boolPrint "global" global,
          boolPrint "user" user,
          boolPrint "site" site
        ]
