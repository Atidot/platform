{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Platform.Packaging.Pip.Types.Input 
    ( VersOrdering(..)
    , ReqSpec(..)
    , PkgVersion(..)
    , PipInput(..)
    , FileList
    , URLList
    , PkgList
    )
where

import           "base"         Data.Data (Data)
import           "base"         Data.Typeable (Typeable)
import           "base"         GHC.Generics (Generic)
import           "aeson"        Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import           "text"         Data.Text (Text)
import qualified "text"         Data.Text as T
import           "data-default" Data.Default (Default, def)
import Platform.Packaging.Pip.Types.Utils

data VersOrdering
  = PipGT
  | PipGTorEq
  | PipEq
  | PipLT
  | PipLTorEq
  | PipNEq
  | PipCompatEq
  | PipArbitraryEq
  deriving (Read, Eq, Ord, Enum, Data, Typeable, Generic)

instance ToJSON VersOrdering where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON VersOrdering where

instance Show VersOrdering where
  show PipGT = ">"
  show PipGTorEq = ">="
  show PipEq = "=="
  show PipLT = "<"
  show PipLTorEq = "<="
  show PipNEq = "!="
  show PipCompatEq = "~="
  show PipArbitraryEq = "==="

data PkgVersion
  = VersMaj !Int
  | VersMin !Int !Int
  | VersIncr !Int !Int !Int
  | VersArb !Text
  deriving (Read, Eq, Ord, Data, Typeable, Generic)

instance ToJSON PkgVersion where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON PkgVersion where

instance Show PkgVersion where
  show (VersMaj n) = show n
  show (VersMin m n) = show m <> "." <> show n
  show (VersIncr l m n) = show l <> "." <> show m <> "." <> show n
  show (VersArb t) = show t

data ReqSpec
  = ReqSpec
  { _reqSpec_pkgName :: !Text
  , _reqSpec_version :: !(Maybe (VersOrdering, PkgVersion))
  }
  deriving (Read, Eq, Ord, Data, Typeable, Generic)

instance ToJSON ReqSpec where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ReqSpec where

instance Show ReqSpec where
  show (ReqSpec t Nothing) = T.unpack t
  show (ReqSpec t (Just (ord, vers))) = T.unpack t <> show ord <> show vers

type FileList = [FilePathT]

type URLList = [URL]

type PkgList = [Text]

data PipInput
  = URLInput URLList
  | FileInput FileList
  | ReqSpecInput [ReqSpec]
  deriving (Read, Show, Eq, Ord, Data, Typeable, Generic)

instance ToJSON PipInput where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON PipInput where

instance Input PipInput where
  fmtInput (URLInput urls) = urls
  fmtInput (FileInput fps) = fps
  fmtInput (ReqSpecInput reqsList) = fmtReqs reqsList
    where
      fmtReqs = noEmpty . (map $ T.pack . show)
