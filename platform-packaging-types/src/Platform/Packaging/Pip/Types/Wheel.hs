{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Platform.Packaging.Pip.Types.Wheel (WheelOpts(..)) where

import           "base"         Data.Data (Data)
import           "base"         Data.Typeable (Typeable)
import           "base"         GHC.Generics (Generic)
import           "aeson"        Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import           "text"         Data.Text (Text)
import           "data-default" Data.Default (Default, def)
import Platform.Packaging.Pip.Types.Utils

data WheelOpts
  = WheelOpts
      { _wheelOpts_wheelDir :: !(Maybe FilePathT),
        _wheelOpts_noBinary :: !(Maybe FormatControl),
        _wheelOpts_onlyBinary :: !(Maybe FormatControl),
        _wheelOpts_preferBinary :: !(Maybe Bool),
        _wheelOpts_buildOption :: !(Maybe Text),
        _wheelOpts_noBuildIsolation :: !(Maybe Bool),
        _wheelOpts_usePEP517 :: !(Maybe Bool),
        _wheelOpts_noUsePEP517 :: !(Maybe Bool),
        _wheelOpts_constraint :: !(Maybe FilePathT),
        _wheelOpts_editable :: !(Maybe (Either FilePathT URL)),
        _wheelOpts_requirement :: !(Maybe FilePathT),
        _wheelOpts_src :: !(Maybe FilePathT),
        _wheelOpts_ignoreRequiresPython :: !(Maybe Bool),
        _wheelOpts_noDeps :: !(Maybe Bool),
        _wheelOpts_build :: !(Maybe FilePathT),
        _wheelOpts_progressBar :: !(Maybe ProgressBar),
        _wheelOpts_globalOption :: !(Maybe Text),
        _wheelOpts_pre :: !(Maybe Bool),
        _wheelOpts_noClean :: !(Maybe Bool),
        _wheelOpts_requireHashes :: !(Maybe Bool),
        _wheelOpts_index :: !(Maybe URL),
        _wheelOpts_extraIndex :: !(Maybe URL),
        _wheelOpts_noIndex :: !(Maybe Bool),
        _wheelOpts_findLinks :: !(Maybe URL)
      }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance ToJSON WheelOpts where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON WheelOpts where

instance Default WheelOpts where
  def =
    WheelOpts
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
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing

instance Opts WheelOpts where
  fmtOpts
    ( WheelOpts
        wheelDir
        noBinary
        onlyBinary
        preferBinary
        buildOption
        noBuildIsolation
        usePEP517
        noUsePEP517
        constraint
        editable
        requirement
        src
        ignoreRequiresPython
        noDeps
        build
        progressBar
        globalOption
        pre
        noClean
        requireHashes
        index
        extraIndex
        noIndex
        findLinks
      ) =
      noEmpty
        [ txtPrint "wheel-dir" wheelDir,
          controlPrint "no-binary" noBinary,
          controlPrint "only-binary" onlyBinary,
          boolPrint "prefer-binary" preferBinary,
          txtPrint "build-option" buildOption,
          boolPrint "no-build-isolation" noBuildIsolation,
          boolPrint "use-pep517" usePEP517,
          boolPrint "no-use-pep517" noUsePEP517,
          txtPrint "constraint" constraint,
          eitherPrint "editable" editable,
          txtPrint "requirement" requirement,
          txtPrint "src" src,
          boolPrint "ignore-requires-python" ignoreRequiresPython,
          boolPrint "no-deps" noDeps,
          txtPrint "build" build,
          progressPrint progressBar,
          txtPrint "global-option" globalOption,
          boolPrint "pre" pre,
          boolPrint "no-clean" noClean,
          boolPrint "require-hashes" requireHashes,
          txtPrint "index" index,
          txtPrint "extra-index" extraIndex,
          boolPrint "no-index" noIndex,
          txtPrint "find-links" findLinks
        ]
