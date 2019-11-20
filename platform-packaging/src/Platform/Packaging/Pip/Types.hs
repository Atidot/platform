{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
module Platform.Packaging.Pip.Types
  ( fmtOpts,
    fmtInput,
    Opts,
    Input,
    UpgradeStrategy,
    FormatControl,
    ProgressBar,
    GeneralOpts,
    InstallOpts,
    DownloadOpts,
    UninstallOpts,
    FreezeOpts,
    ListOpts,
    ShowOpts,
    SearchOpts,
    CheckOpts,
    ConfigOpts,
    WheelOpts,
    HashOpts,
    DebugOpts,
    PipInput,
    ConfigInput,
  )
where

import "base" Data.Data (Data)
import "base" Data.Typeable (Typeable)
import "base" GHC.Generics (Generic)
import "aeson" Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import "text" Data.Text (Text)
import qualified "text" Data.Text as T
import "data-default" Data.Default (Default, def)

type URL = Text

class Opts a where
  fmtOpts :: a -> [Text]

data UpgradeStrategy
  = Eager
  | OnlyIfNeeded
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

instance ToJSON UpgradeStrategy where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON UpgradeStrategy where

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

data Action
  = SwitchAction
  | IgnoreAction
  | WipeAction
  | BackupAction
  | AbortAction
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

instance ToJSON  where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON  where

data GeneralOpts
  = GeneralOpts
      { _generalOpts_help :: !(Maybe Bool)
      , _generalOpts_isolated :: !(Maybe Bool)
      , _generalOpts_verbose :: !(Maybe Bool)
      , _generalOpts_version :: !(Maybe Bool)
      , _generalOpts_quiet :: !(Maybe Bool)
      , _generalOpts_log :: !(Maybe FilePath)
      , _generalOpts_proxy :: !(Maybe Text)
      , _generalOpts_retries :: !(Maybe Int)
      , _generalOpts_timeout :: !(Maybe Int)
      , _generalOpts_existsAction :: !(Maybe Action)
      , _generalOpts_trustedHost :: !(Maybe URL)
      , _generalOpts_cert :: !(Maybe FilePath)
      , _generalOpts_clientCert :: !(Maybe FilePath)
      , _generalOpts_cacheDir :: !(Maybe FilePath)
      , _generalOpts_noCacheDir :: !(Maybe Bool)
      , _generalOpts_disablePipVersionCheck :: !(Maybe Bool)
      , _generalOpts_noColor :: !(Maybe Bool)
      } 
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance ToJSON  where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON  where

instance Default GeneralOpts where
    def = Nothing
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

instance Opts GeneralOpts where
    fmtOpts (GeneralOpts help
                         isolated
                         verbose
                         version
                         quiet
                         log
                         proxy
                         retries
                         timeout
                         existsAction
                         trustedHost
                         cert
                         clientCert
                         cacheDir
                         noCacheDir
                         disablePipVersionCheck
                         noColor)
                           = noEmpty [ boolPrint "help" help
                                     , boolPrint "isolated" isolated
                                     , boolPrint "verbose" verbose
                                     , boolPrint "version" version
                                     , boolPrint "quiet" quiet
                                     , txtPrint "log" log
                                     , txtPrint "proxy" proxy
                                     , intPrint "retries" retries
                                     , intPrint "timeout" timeout
                                     , actionPrint "exists-action" existsAction
                                     , txtPrint "trusted-host" trustedHost
                                     , txtPrint "cert" cert
                                     , txtPrint "client-cert" clientCert
                                     , txtPrint "cache-dir" cacheDir
                                     , boolPrint "no-cache-dir" noCacheDir
                                     , boolPrint "disable-pip-version-check" disablePipVersionCheck
                                     , boolPrint "no-color" noColor
                                     ]

instance ToJSON  where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON  where

data InstallOpts
  = InstallOpts
      { _installOpts_requirement :: !(Maybe FilePath),
        _installOpts_constraint :: !(Maybe FilePath),
        _installOpts_noDeps :: !(Maybe Bool),
        _installOpts_pre :: !(Maybe Bool),
        _installOpts_editable :: !(Maybe (Either FilePath URL)),
        _installOpts_target :: !(Maybe FilePath),
        _installOpts_platform :: !(Maybe Text),
        _installOpts_pythonVersion :: !(Maybe Text),
        _installOpts_implementation :: !(Maybe Text),
        _installOpts_abi :: !(Maybe Text),
        _installOpts_user :: !(Maybe Bool),
        _installOpts_root :: !(Maybe FilePath),
        _installOpts_prefix :: !(Maybe FilePath),
        _installOpts_build :: !(Maybe FilePath),
        _installOpts_src :: !(Maybe FilePath),
        _installOpts_upgrade :: !(Maybe Bool),
        _installOpts_upgradeStrategy :: !(Maybe Text),
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
        requiresHashes
        progressBar
        index
        extraIndex
        noIndex
        findLinks
      ) =
      noEmpty
        [ txtPrint "requirement" requirement,
          txtPrint "constraint" constraint,
          boolPrint "no-deps" no - deps,
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
          "--progress-bar" ++ progressPrint progressBar,
          txtPrint "index-url" index,
          txtPrint "extra-index-url" extraIndex,
          boolPrint "no-index" noIndex,
          txtPrint "find-links" findLinks
        ]
      where
        upgrPrint Eager = "--upgrade-strategy eager"
        upgrPrint OnlyIfNeeded = "--upgrade-strategy only-if-needed"

instance ToJSON  where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON  where

data DownloadOpts
  = DownloadOpts
      { _downloadOpts_constraint :: !(Maybe FilePath),
        _downloadOpts_requirement :: !(Maybe FilePath),
        _downloadOpts_build :: !(Maybe FilePath),
        _downloadOpts_noDeps :: !(Maybe Bool),
        _downloadOpts_noBinary :: !(Maybe FormatControl),
        _downloadOpts_onlyBinary :: !(Maybe FormatControl),
        _downloadOpts_preferBinary :: !(Maybe Bool),
        _downloadOpts_src :: !(Maybe FilePath),
        _downloadOpts_pre :: !(Maybe Bool),
        _downloadOpts_noClean :: !(Maybe Bool),
        _downloadOpts_requireHashes :: !(Maybe Bool),
        _downloadOpts_progressBar :: !(Maybe ProgressBar),
        _downloadOpts_noBuildIsolation :: !(Maybe Bool),
        _downloadOpts_usePEP517 :: !(Maybe Bool),
        _downloadOpts_noUsePEP517 :: !(Maybe Bool),
        _downloadOpts_dest :: !(Maybe FilePath),
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

instance Default DownloadOpts where
  def =
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
          txtPrint "index" index,
          txtPrint "extra-index" extraIndex,
          boolPrint "no-index" noIndex,
          txtPrint "find-links" findLinks
        ]

instance ToJSON  where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON  where

data UninstallOpts
  = UninstallOpts
      { _uninstallOpts_requirement :: !(Maybe FilePath),
        _uninstallOpts_yes :: !(Maybe Bool)
      }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance Default UninstallOpts where
  def = Nothing Nothing

instance Opts DownloadOpts where
  fmtOpts (UninstallOpts requirement yes) =
    noEmpty [txtPrint "requirement" requirement, boolPrint "yes" yes]

instance ToJSON  where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON  where

data FreezeOpts
  = FreezeOpts
      { _freezeOpts_requirements :: !(Maybe FilePath),
        _freezeOpts_findLinks :: !(Maybe URL),
        _freezeOpts_local :: !(Maybe Bool),
        _freezeOpts_user :: !(Maybe Bool),
        _freezeOpts_path :: !(Maybe FilePath),
        _freezeOpts_all :: !(Maybe Bool),
        _freezeOpts_excludeEditable :: !(Maybe Bool)
      }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance Default FreezeOpts where
  def =
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

instance ToJSON  where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON  where

data Format
  = Columns
  | Freeze
  | JSON
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

instance ToJSON  where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON  where

data ListOpts
  = ListOpts
      { _listOpts_outdated :: !(Maybe Bool),
        _listOpts_uptodate :: !(Maybe Bool),
        _listOpts_editable :: !(Maybe Bool),
        _listOpts_local :: !(Maybe Bool),
        _listOpts_user :: !(Maybe Bool),
        _listOpts_path :: !(Maybe FilePath),
        _listOpts_pre :: !(Maybe Bool),
        _listOpts_format :: !(Maybe Format),
        _listOpts_notRequired :: !(Maybe Bool),
        _listOpts_excludeEditable :: !(Maybe Bool),
        _listOpts_includeEditable :: !(Maybe Bool),
        _listOpts_index :: !(Maybe URL),
        _listOpts_extraIndex :: !(Maybe URL),
        _listOpts_noIndex :: !(Maybe Bool),
        _listOpts_findLinks :: !(Maybe URL)
      }
  deriving (Read, Eq, Ord, Data, Typeable, Generic)

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

instance ToJSON  where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON  where

newtype ShowOpts
  = ShowOpts
      {_showOpts_files :: !(Maybe Bool)}
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

instance Default ShowOpts where
  def = ShowOpts Nothing

instance Opts ShowOpts where
  fmtOpts = noEmpty [txtPrint "files" files]

instance ToJSON  where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON  where

newtype SearchOpts
  = SearchOpts
      {_searchOpts_index :: !(Maybe URL)}
  deriving (Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

instance Default SearchOpts where
  def = SearchOpts Nothing

instance Opts SearchOpts where
  fmtOpts (SearchOpts index) = noEmpty [txtPrint "index" index]

instance ToJSON  where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON  where

newtype CheckOpts = CheckOpts ()
  deriving (Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

instance Default CheckOpts where
  def = CheckOpts ()

instance Opts CheckOpts where
  fmtOpts _ = ""

instance ToJSON  where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON  where

data ConfigOpts
  = ConfigOpts
      { _configOpts_editor :: !(Maybe Text),
        _configOpts_global :: !(Maybe Bool),
        _configOpts_user :: !(Maybe Bool),
        _configOpts_site :: !(Maybe Bool)
      }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

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

instance ToJSON  where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON  where

data WheelOpts
  = WheelOpts
      { _wheelOpts_wheelDir :: !(Maybe FilePath),
        _wheelOpts_noBinary :: !(Maybe FormatControl),
        _wheelOpts_onlyBinary :: !(Maybe FormatControl),
        _wheelOpts_preferBinary :: !(Maybe Bool),
        _wheelOpts_buildOption :: !(Maybe Text),
        _wheelOpts_noBuildIsolation :: !(Maybe Bool),
        _wheelOpts_usePEP517 :: !(Maybe Bool),
        _wheelOpts_noUsePEP517 :: !(Maybe Bool),
        _wheelOpts_constraint :: !(Maybe FilePath),
        _wheelOpts_editable :: !(Maybe (Either FilePath URL)),
        _wheelOpts_requirement :: !(Maybe FilePath),
        _wheelOpts_src :: !(Maybe FilePath),
        _wheelOpts_ignoreRequiresPython :: !(Maybe Bool),
        _wheelOpts_noDeps :: !(Maybe Bool),
        _wheelOpts_build :: !(Maybe FilePath),
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
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

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

instance ToJSON  where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON  where

data HashOpts
  = SHA256
  | SHA384
  | SHA512
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

instance Default HashOpts where
  def = SHA256

instance Opts HashOpts where
  fmtOpts opt = [txtPrint "algorithm" (algName opt)]
    where
      algName SHA256 = "sha256"
      algName SHA384 = "sha384"
      algName SHA512 = "sha512"

instance ToJSON  where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON  where

data DebugOpts
  = DebugOpts
      { _debugOpts_platform :: !(Maybe Text),
        _debugOpts_pythonVersion :: !(Maybe Text),
        _debugOpts_implementation :: !(Maybe Text),
        _debugOpts_abi :: !(Maybe Text)
      }
  deriving (Read, Show, Eq, Ord, Data, Typeable, Generic)

instance Default DebugOpts where
  def = Nothing Nothing Nothing Nothing

instance Opts DebugOpts where
  fmtOpts (DebugOpts plat vers impl abi) =
    noEmpty
      [ txtPrint "platform" plat,
        txtPrint "python-version" vers,
        txtPrint "implementation" impl,
        txtPrint "abi" abi
      ]

install = undefined

instance ToJSON  where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON  where

newtype ReqSpec = ReqSpec ()

instance ToJSON  where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON  where

newtype PkgIndexOpts = PkgIndexOpts ()

instance ToJSON  where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON  where

type FileList = [FilePath]

type URLList = [URL]

type PkgList = [Text]

class Input a where
  fmtInput :: a -> [Text]

data PipInput
  = URLInput URLList
  | FileInput FileList
  | ReqSpecInput [(ReqSpec, PkgIndexOpts)]
  | ReqFileInput [(FilePath, PkgIndexOpts)]
  deriving (Read, Show, Eq, Ord, Data, Typeable, Generic)

instance Input PipInput where
  fmtInput (URLInput urls) = urls
  fmtInput (FileInput fps) = fps
  fmtInput (ReqSpecInput reqsList) = fmtReqs reqsList
  fmtInput (ReqFileInput reqsList) = fmtReqs reqsList
    where
      fmtReqs = noEmpty . concat $ map (\(r, p) -> [r, fmtOpts p])

instance ToJSON  where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON  where

data UninstallInput
  = UnInstPkgs PkgList
  | UnInstFiles FileList
  deriving (Read, Show, Eq, Ord, Data, Typeable, Generic)

instance Input UninstallInput where
  fmtInput (UnInstPkgs pkgs) = pkgs
  fmtInput (UnInstFiles files) = files

instance ToJSON  where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON  where

data ConfigInput
  = List
  | Edit
  | Get Text
  | Set Text Text
  | Unset Text

instance Input ConfigInput where
  fmtInput List = ["list"]
  fmtInput Edit = ["edit"]
  fmtInput Get name = ["get", name]
  fmtInput Set name value = ["set", name, value]
  fmtInput Unset name = ["unset", name]

instance ToJSON  where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON  where

txtPrint :: Text 
         -> Maybe Text 
         -> Text
txtPrint _ Nothing = ""
txtPrint optName optVar = "--" ++ optName ++ " " ++ optVar

boolPrint :: Text
          -> Maybe Bool
          -> Text
boolPrint _ Nothing = ""
boolPrint _ (Just False) = ""
boolPrint optName (Just True) = "--" ++ optName

actionPrint :: Action -> Text
actionPrint Switch = "s"
actionPrint Ignore = "i"
actionPrint Wipe = "w"
actionPrint Backup = "b"
actionPrint Abort = "a"

controlPrint :: Text 
             -> Maybe FormatControl
             -> Text
controlPrint _ Nothing = ""
controlPrint name (Just All) = "--" ++ name ++ " :all:"
controlPrint name (Just None) = "--" ++ name ++ " :none:"
controlPrint name (Just (Pkgs ps)) = "--" ++ name ++ " " ++ 
  intercalate "," ps

progressPrint :: Maybe ProgressBar -> Text
progressPrint Nothing = ""
progressPrint (Just bar) = "--progress-bar " ++ (T.pack . toLower . show) bar

eitherPrint :: Text -> Maybe (Either FilePath URL) -> Text
eitherPrint _ Nothing = ""
eitherPrint name (Just (Left t)) = "--" ++ name ++ " " ++ t
eitherPrint name (Just (Right t)) = "--" ++ name ++ " " ++ t

noEmpty :: [Text] -> [Text]
noEmpty = filter (/= "")
