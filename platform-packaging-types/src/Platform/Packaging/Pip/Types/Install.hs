{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Platform.Packaging.Pip.Types.Install (UpgradeStrategy(..), InstallOpts(..)) where

import           "base"         Data.Data (Data)
import           "base"         Data.Typeable (Typeable)
import           "base"         GHC.Generics (Generic)
import           "aeson"        Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import           "text"         Data.Text (Text)
import           "data-default" Data.Default (Default, def)
import Platform.Packaging.Pip.Types.Utils

data UpgradeStrategy
  = Eager
  | OnlyIfNeeded
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

instance ToJSON UpgradeStrategy where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON UpgradeStrategy where

data InstallOpts
  = InstallOpts
      { _installOpts_requirement :: !(Maybe FilePathT),
        _installOpts_constraint :: !(Maybe FilePathT),
        _installOpts_noDeps :: !(Maybe Bool),
        _installOpts_pre :: !(Maybe Bool),
        _installOpts_editable :: !(Maybe (Either FilePathT URL)),
        _installOpts_target :: !(Maybe FilePathT),
        _installOpts_platform :: !(Maybe Text),
        _installOpts_pythonVersion :: !(Maybe Text),
        _installOpts_implementation :: !(Maybe Text),
        _installOpts_abi :: !(Maybe Text),
        _installOpts_user :: !(Maybe Bool),
        _installOpts_root :: !(Maybe FilePathT),
        _installOpts_prefix :: !(Maybe FilePathT),
        _installOpts_build :: !(Maybe FilePathT),
        _installOpts_src :: !(Maybe FilePathT),
        _installOpts_upgrade :: !(Maybe Bool),
        _installOpts_upgradeStrategy :: !(Maybe UpgradeStrategy),
        _installOpts_forceReinstall :: !(Maybe Bool),
        _installOpts_ignoreInstalled :: !(Maybe Bool),
        _installOpts_ignoreRequiresPython :: !(Maybe Bool),
        _installOpts_noBuildIsolation :: !(Maybe Bool),
        _installOpts_usePEP517 :: !(Maybe Bool),
        _installOpts_noUsePEP517 :: !(Maybe Bool),
        _installOpts_installOption :: !(Maybe Text),
        _installOpts_globalOption :: !(Maybe Text),
        _installOpts_compile :: !(Maybe Bool),
        _installOpts_noCompile :: !(Maybe Bool),
        _installOpts_noWarnScriptLocation :: !(Maybe Bool),
        _installOpts_noWarnConflicts :: !(Maybe Bool),
        _installOpts_noBinary :: !(Maybe FormatControl),
        _installOpts_onlyBinary :: !(Maybe FormatControl),
        _installOpts_preferBinary :: !(Maybe Bool),
        _installOpts_noClean :: !(Maybe Bool),
        _installOpts_requireHashes :: !(Maybe Bool),
        _installOpts_progressBar :: !(Maybe ProgressBar),
        _installOpts_index :: !(Maybe URL),
        _installOpts_extraIndex :: !(Maybe URL),
        _installOpts_noIndex :: !(Maybe Bool),
        _installOpts_findLinks :: !(Maybe URL)
      }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance ToJSON InstallOpts where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON InstallOpts where

instance Default InstallOpts where
  def =
    InstallOpts
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

instance Opts InstallOpts where
  fmtOpts
    ( InstallOpts
        requirement
        constraint
        noDeps
        pre
        editable
        target
        platform
        pythonVersion
        implementation
        abi
        user
        root
        prefix
        build
        src
        upgrade
        upgradeStrategy
        forceReinstall
        ignoreInstalled
        ignoreRequiresPython
        noBuildIsolation
        usePEP517
        noUsePEP517
        installOption
        globalOption
        compile
        noCompile
        noWarnScriptLocation
        noWarnConflicts
        noBinary
        onlyBinary
        preferBinary
        noClean
        requireHashes
        progressBar
        index
        extraIndex
        noIndex
        findLinks
      ) =
      noEmpty
        [ txtPrint "requirement" requirement,
          txtPrint "constraint" constraint,
          boolPrint "no-deps" noDeps,
          boolPrint "pre" pre,
          eitherPrint "editable" editable,
          txtPrint "target" target,
          txtPrint "platform" platform,
          txtPrint "python-version" pythonVersion,
          txtPrint "implementation" implementation,
          txtPrint "abi" abi,
          boolPrint "user" user,
          txtPrint "root" root,
          txtPrint "prefix" prefix,
          txtPrint "build" build,
          txtPrint "src" src,
          boolPrint "upgrade" upgrade,
          upgrPrint upgradeStrategy,
          boolPrint "force-reinstall" forceReinstall,
          boolPrint "ignore-installed" ignoreInstalled,
          boolPrint "ignore-requires-python" ignoreRequiresPython,
          boolPrint "no-build-isolation" noBuildIsolation,
          boolPrint "use-pep517" usePEP517,
          boolPrint "no-use-pep517" noUsePEP517,
          txtPrint "install-option" installOption,
          txtPrint "global-option" globalOption,
          boolPrint "compile" compile,
          boolPrint "no-compile" noCompile,
          boolPrint "no-warn-script-location" noWarnScriptLocation,
          boolPrint "no-warn-conflicts" noWarnConflicts,
          controlPrint "no-binary" noBinary,
          controlPrint "only-binary" onlyBinary,
          boolPrint "prefer-binary" preferBinary,
          boolPrint "no-clean" noClean,
          boolPrint "require-hashes" requireHashes,
          progressPrint progressBar,
          txtPrint "index-url" index,
          txtPrint "extra-index-url" extraIndex,
          boolPrint "no-index" noIndex,
          txtPrint "find-links" findLinks
        ]
      where
        upgrPrint Nothing = ""
        upgrPrint (Just Eager) = "--upgrade-strategy eager"
        upgrPrint (Just OnlyIfNeeded) = "--upgrade-strategy only-if-needed"
