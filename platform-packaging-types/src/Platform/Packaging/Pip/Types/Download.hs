{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Platform.Packaging.Pip.Types.Download (DownloadOpts(..)) where

import "base" Data.Data (Data)
import "base" Data.Typeable (Typeable)
import "base" GHC.Generics (Generic)
import "aeson" Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import "text" Data.Text (Text)
import "data-default" Data.Default (Default, def)
import Platform.Packaging.Pip.Types.Utils

data DownloadOpts
  = DownloadOpts
      { _downloadOpts_constraint :: !(Maybe FilePathT),
        _downloadOpts_requirement :: !(Maybe FilePathT),
        _downloadOpts_build :: !(Maybe FilePathT),
        _downloadOpts_noDeps :: !(Maybe Bool),
        _downloadOpts_noBinary :: !(Maybe FormatControl),
        _downloadOpts_onlyBinary :: !(Maybe FormatControl),
        _downloadOpts_preferBinary :: !(Maybe Bool),
        _downloadOpts_src :: !(Maybe FilePathT),
        _downloadOpts_pre :: !(Maybe Bool),
        _downloadOpts_noClean :: !(Maybe Bool),
        _downloadOpts_requireHashes :: !(Maybe Bool),
        _downloadOpts_progressBar :: !(Maybe ProgressBar),
        _downloadOpts_noBuildIsolation :: !(Maybe Bool),
        _downloadOpts_usePEP517 :: !(Maybe Bool),
        _downloadOpts_noUsePEP517 :: !(Maybe Bool),
        _downloadOpts_dest :: !(Maybe FilePathT),
        _downloadOpts_platform :: !(Maybe Text),
        _downloadOpts_pythonVersion :: !(Maybe Text),
        _downloadOpts_implementation :: !(Maybe Text),
        _downloadOpts_abi :: !(Maybe Text),
        _downloadOpts_index :: !(Maybe URL),
        _downloadOpts_extraIndex :: !(Maybe URL),
        _downloadOpts_noIndex :: !(Maybe Bool),
        _downloadOpts_findLinks :: !(Maybe URL)
      }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance ToJSON DownloadOpts where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON DownloadOpts where

instance Default DownloadOpts where
  def =
    DownloadOpts
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

instance Opts DownloadOpts where
  fmtOpts
    ( DownloadOpts
        constraint
        requirement
        build
        noDeps
        noBinary
        onlyBinary
        preferBinary
        src
        pre
        noClean
        requireHashes
        progressBar
        noBuildIsolation
        usePEP517
        noUsePEP517
        dest
        platform
        pythonVersion
        implementation
        abi
        index
        extraIndex
        noIndex
        findLinks
      ) =
      noEmpty
        [ txtPrint "constraint" constraint,
          txtPrint "requirement" requirement,
          txtPrint "build" build,
          boolPrint "no-deps" noDeps,
          controlPrint "no-binary" noBinary,
          controlPrint "only-binary" onlyBinary,
          boolPrint "prefer-binary" preferBinary,
          txtPrint "src" src,
          boolPrint "pre" pre,
          boolPrint "no-clean" noClean,
          boolPrint "require-hashes" requireHashes,
          progressPrint progressBar,
          boolPrint "no-build-isolation" noBuildIsolation,
          boolPrint "use-pep517" usePEP517,
          boolPrint "no-use-pep517" noUsePEP517,
          txtPrint "dest" dest,
          txtPrint "platform" platform,
          txtPrint "python-version" pythonVersion,
          txtPrint "implementation" implementation,
          txtPrint "abi" abi,
          txtPrint "index-url" index,
          txtPrint "extra-index" extraIndex,
          boolPrint "no-index" noIndex,
          txtPrint "find-links" findLinks
        ]
