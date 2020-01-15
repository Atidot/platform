{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Platform.Packaging.Pip.Types.Hash (HashAlgs(..), HashOpts(..)) where

import           "base"         Data.Data (Data)
import           "base"         Data.Typeable (Typeable)
import           "base"         GHC.Generics (Generic)
import           "aeson"        Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import           "text"         Data.Text (Text)
import           "data-default" Data.Default (Default, def)
import Platform.Packaging.Pip.Types.Utils

data HashAlgs
  = SHA256
  | SHA384
  | SHA512
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

instance ToJSON HashAlgs where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON HashAlgs where

data HashOpts
  = HashOpts {_hashOpts_hashAlgs :: !(Maybe HashAlgs)}
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance ToJSON HashOpts where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON HashOpts where

instance Default HashOpts where
  def = HashOpts Nothing

instance Opts HashOpts where
  fmtOpts (HashOpts hashAlgs)  = noEmpty [txtPrint "algorithm" (fmap algName hashAlgs)]
    where
      algName SHA256 = "sha256"
      algName SHA384 = "sha384"
      algName SHA512 = "sha512"
