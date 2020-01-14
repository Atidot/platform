{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Platform.Packaging.Pip.Types.Utils
  ( URL
  , FilePathT
  , Opts
  , fmtOpts
  , Input
  , fmtInput
  , Action
  , FormatControl
  , ProgressBar
  , OutputFormat
  , ifJust
  , optPrint
  , txtPrint
  , boolPrint
  , actionPrint
  , controlPrint
  , progressPrint
  , formatPrint
  , eitherPrint
  , intPrint
  , noEmpty
  )
where

import           "base"         Data.Char (toLower)
import           "base"         Data.Data (Data)
import           "base"         Data.Typeable (Typeable)
import           "base"         GHC.Generics (Generic)
import           "aeson"        Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import           "text"         Data.Text (Text, intercalate)
import qualified "text"         Data.Text as T
import           "data-default" Data.Default (Default, def)

type URL = Text
type FilePathT = Text

class Opts a where
  fmtOpts :: a -> [Text]

class Input a where
  fmtInput :: a -> [Text]

data Action
  = SwitchAction
  | IgnoreAction
  | WipeAction
  | BackupAction
  | AbortAction
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

instance ToJSON Action where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Action where

data FormatControl
  = None
  | All
  | Pkgs [Text]
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance ToJSON FormatControl where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON FormatControl where

data ProgressBar
  = Off
  | On
  | ASCII
  | Pretty
  | Emoji
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

instance ToJSON ProgressBar where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ProgressBar where

data OutputFormat
  = Columns
  | Freeze
  | JSON
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

instance ToJSON OutputFormat where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON OutputFormat

ifJust :: Maybe Text -> Text
ifJust Nothing = ""
ifJust (Just t) = t

optPrint :: (a -> Text)
         -> Text
         -> Maybe a
         -> Text
optPrint _ _ Nothing = ""
optPrint f t (Just x) = "--" <> t <> f x

txtPrint :: Text
         -> Maybe Text
         -> Text
txtPrint = optPrint (\t -> " " <> t)

boolPrint :: Text
          -> Maybe Bool
          -> Text
boolPrint t (Just True) = "--" <> t
boolPrint _ _ = ""

actionPrint :: Text
            -> Maybe Action
            -> Text
actionPrint = optPrint (\a -> " " <> ap a)
  where ap SwitchAction = "s"
        ap IgnoreAction = "i"
        ap WipeAction = "w"
        ap BackupAction = "b"
        ap AbortAction = "a"

controlPrint :: Text
             -> Maybe FormatControl
             -> Text
controlPrint = optPrint (\c -> " " <> cp c)
    where cp All = ":all:"
          cp None = ":none:"
          cp (Pkgs ps) = intercalate "," ps

progressPrint :: Maybe ProgressBar -> Text
progressPrint = optPrint (\b -> " " <> pb b) "progress-bar"
    where pb = T.pack . map toLower . show

formatPrint :: Text
            -> Maybe OutputFormat
            -> Text
formatPrint = optPrint (\f -> " " <> fp f)
    where fp = T.pack . map toLower . show

eitherPrint :: Text
            -> Maybe (Either FilePathT URL)
            -> Text
eitherPrint = optPrint (\e -> " " <> ep e)
    where ep (Left t) = t
          ep (Right t) = t

intPrint :: Text
         -> Maybe Int
         -> Text
intPrint = optPrint (\n -> " " <> np n)
    where np = T.pack . show

noEmpty :: [Text] -> [Text]
noEmpty = filter (/= "")
