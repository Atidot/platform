{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Platform.Packaging.Pip.Types
  ( fmtOpts,
    fmtInput,
    Opts,
    Input,
    UpgradeStrategy,
    FormatControl,
    ProgressBar,
    HashAlgs,
    OutputFormat,
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
    UninstallInput,
    ConfigInput,
    PkgList,
    FileList
  )
where

import "base" Data.Char (toLower)
import "base" Data.Data (Data)
import "base" Data.Typeable (Typeable)
import "base" GHC.Generics (Generic)
import "text" Data.Text (Text, intercalate)
import qualified "text" Data.Text as T
import "data-default" Data.Default (Default, def)

type URL = Text
type FilePathT = Text

class Opts a where
  fmtOpts :: a -> [Text]

data UpgradeStrategy
  = Eager
  | OnlyIfNeeded
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

data FormatControl
  = None
  | All
  | Pkgs [Text]
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

data ProgressBar
  = Off
  | On
  | ASCII
  | Pretty
  | Emoji
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

data Action
  = SwitchAction
  | IgnoreAction
  | WipeAction
  | BackupAction
  | AbortAction
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

data GeneralOpts
  = GeneralOpts
      { _generalOpts_help :: !(Maybe Bool)
      , _generalOpts_isolated :: !(Maybe Bool)
      , _generalOpts_verbose :: !(Maybe Bool)
      , _generalOpts_version :: !(Maybe Bool)
      , _generalOpts_quiet :: !(Maybe Bool)
      , _generalOpts_log :: !(Maybe FilePathT)
      , _generalOpts_proxy :: !(Maybe Text)
      , _generalOpts_retries :: !(Maybe Int)
      , _generalOpts_timeout :: !(Maybe Int)
      , _generalOpts_existsAction :: !(Maybe Action)
      , _generalOpts_trustedHost :: !(Maybe URL)
      , _generalOpts_cert :: !(Maybe FilePathT)
      , _generalOpts_clientCert :: !(Maybe FilePathT)
      , _generalOpts_cacheDir :: !(Maybe FilePathT)
      , _generalOpts_noCacheDir :: !(Maybe Bool)
      , _generalOpts_disablePipVersionCheck :: !(Maybe Bool)
      , _generalOpts_noColor :: !(Maybe Bool)
      } 
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance Default GeneralOpts where
    def = GeneralOpts Nothing
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
          txtPrint "index" index,
          txtPrint "extra-index" extraIndex,
          boolPrint "no-index" noIndex,
          txtPrint "find-links" findLinks
        ]

data UninstallOpts
  = UninstallOpts
      { _uninstallOpts_requirement :: !(Maybe FilePathT),
        _uninstallOpts_yes :: !(Maybe Bool)
      }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance Default UninstallOpts where
  def = UninstallOpts Nothing Nothing

instance Opts UninstallOpts where
  fmtOpts (UninstallOpts requirement yes) =
    noEmpty [txtPrint "requirement" requirement, boolPrint "yes" yes]

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

data OutputFormat
  = Columns
  | Freeze
  | JSON
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

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

data ShowOpts
  = ShowOpts
      {_showOpts_files :: !(Maybe Bool)}
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance Default ShowOpts where
  def = ShowOpts Nothing

instance Opts ShowOpts where
  fmtOpts (ShowOpts files) = noEmpty [boolPrint "files" files]

data SearchOpts
  = SearchOpts
      {_searchOpts_index :: !(Maybe URL)}
  deriving (Read, Eq, Ord, Data, Typeable, Generic)

instance Default SearchOpts where
  def = SearchOpts Nothing

instance Opts SearchOpts where
  fmtOpts (SearchOpts index) = noEmpty [txtPrint "index" index]

newtype CheckOpts = CheckOpts ()
  deriving (Read, Eq, Ord, Bounded, Data, Typeable, Generic)

instance Default CheckOpts where
  def = CheckOpts ()

instance Opts CheckOpts where
  fmtOpts _ = []

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

data HashAlgs
  = SHA256
  | SHA384
  | SHA512
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

data HashOpts
  = HashOpts {_hashOpts_hashAlgs :: !(Maybe HashAlgs)}
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance Default HashOpts where
  def = HashOpts Nothing

instance Opts HashOpts where
  fmtOpts (HashOpts hashAlgs)  = noEmpty [txtPrint "algorithm" (fmap algName hashAlgs)]
    where
      algName SHA256 = "sha256"
      algName SHA384 = "sha384"
      algName SHA512 = "sha512"

data DebugOpts
  = DebugOpts
      { _debugOpts_platform :: !(Maybe Text),
        _debugOpts_pythonVersion :: !(Maybe Text),
        _debugOpts_implementation :: !(Maybe Text),
        _debugOpts_abi :: !(Maybe Text)
      }
  deriving (Read, Show, Eq, Ord, Data, Typeable, Generic)

instance Default DebugOpts where
  def = DebugOpts Nothing Nothing Nothing Nothing

instance Opts DebugOpts where
  fmtOpts (DebugOpts plat vers impl abi) =
    noEmpty
      [ txtPrint "platform" plat,
        txtPrint "python-version" vers,
        txtPrint "implementation" impl,
        txtPrint "abi" abi
      ]

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

instance Show ReqSpec where
  show (ReqSpec t Nothing) = T.unpack t
  show (ReqSpec t (Just (ord, vers))) = T.unpack t <> show ord <> show vers

type FileList = [FilePathT]

type URLList = [URL]

type PkgList = [Text]

class Input a where
  fmtInput :: a -> [Text]

data PipInput
  = URLInput URLList
  | FileInput FileList
  | ReqSpecInput [ReqSpec]
  deriving (Read, Show, Eq, Ord, Data, Typeable, Generic)

instance Input PipInput where
  fmtInput (URLInput urls) = urls
  fmtInput (FileInput fps) = fps
  fmtInput (ReqSpecInput reqsList) = fmtReqs reqsList
    where
      fmtReqs = noEmpty . (map $ T.pack . show)

data UninstallInput
  = UnInstPkgs PkgList
  | UnInstFiles FileList
  deriving (Read, Show, Eq, Ord, Data, Typeable, Generic)

instance Input UninstallInput where
  fmtInput (UnInstPkgs pkgs) = pkgs
  fmtInput (UnInstFiles files) = files

data ConfigInput
  = List
  | Edit
  | Get Text
  | Set Text Text
  | Unset Text

instance Input ConfigInput where
  fmtInput List = ["list"]
  fmtInput Edit = ["edit"]
  fmtInput (Get name) = ["get", name]
  fmtInput (Set name value) = ["set", name, value]
  fmtInput (Unset name) = ["unset", name]

ifJust :: Maybe Text -> Text
ifJust Nothing = ""
ifJust (Just t) = t

txtPrint :: Text 
         -> Maybe Text 
         -> Text
txtPrint _ Nothing = ""
txtPrint optName (Just optVar)  = "--" <> optName <> " " <> optVar

boolPrint :: Text
          -> Maybe Bool
          -> Text
boolPrint _ Nothing = ""
boolPrint _ (Just False) = ""
boolPrint optName (Just True) = "--" <> optName

actionPrint :: Text -> Maybe Action -> Text
actionPrint _ Nothing = ""
actionPrint name (Just action) = "--" <> name <> " " <> ap' action
  where ap' SwitchAction = "s"
        ap' IgnoreAction = "i"
        ap' WipeAction = "w"
        ap' BackupAction = "b"
        ap' AbortAction = "a"

controlPrint :: Text 
             -> Maybe FormatControl
             -> Text
controlPrint _ Nothing = ""
controlPrint name (Just All) = "--" <> name <> " :all:"
controlPrint name (Just None) = "--" <> name <> " :none:"
controlPrint name (Just (Pkgs ps)) = "--" <> name <> " " <> 
  intercalate "," ps

progressPrint :: Maybe ProgressBar -> Text
progressPrint Nothing = ""
progressPrint (Just bar) = "--progress-bar " <> (T.pack . (map toLower) . show) bar

formatPrint :: Text -> Maybe OutputFormat -> Text
formatPrint = undefined

eitherPrint :: Text -> Maybe (Either FilePathT URL) -> Text
eitherPrint _ Nothing = ""
eitherPrint name (Just (Left t)) = "--" <> name <> " " <> t
eitherPrint name (Just (Right t)) = "--" <> name <> " " <> t

intPrint :: Text -> Maybe Int -> Text
intPrint _ Nothing = ""
intPrint name (Just n) = "--" <> name <> " " <> (T.pack $ show n)

noEmpty :: [Text] -> [Text]
noEmpty = filter (/= "")