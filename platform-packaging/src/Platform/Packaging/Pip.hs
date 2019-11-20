{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Platform.Packaging.Pip where

import "data-text" Data.Text (Text)
import qualified "data-text" Data.Text as T
import "shellmet"  Shellmet (($|))

class Opts a where
    fmtOpts :: a -> [Text]

data UpgradeStrategy = Eager
                     | OnlyIfNeeded
                     deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

data FormatControl = None
                   | All
                   | Pkgs [Text]
                   deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

data ProgressBar = Off
                 | On
                 | ASCII
                 | Pretty
                 | Emoji
                 deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

data InstallOpts = InstallOpts
    { _installOpts_requirement :: !(Maybe FilePath)
    , _installOpts_constraint :: !(Maybe FilePath)
    , _installOpts_noDeps :: !(Maybe Bool)
    , _installOpts_pre :: !(Maybe Bool)
    , _installOpts_editable :: !(Maybe (Either FilePath URL))
    , _installOpts_target :: !(Maybe FilePath)
    , _installOpts_platform :: !(Maybe Text)
    , _installOpts_pythonVersion :: !(Maybe Text)
    , _installOpts_implementation :: !(Maybe Text)
    , _installOpts_abi :: !(Maybe Text)
    , _installOpts_user :: !(Maybe Bool)
    , _installOpts_root :: !(Maybe FilePath)
    , _installOpts_prefix :: !(Maybe FilePath)
    , _installOpts_build :: !(Maybe FilePath)
    , _installOpts_src :: !(Maybe FilePath)
    , _installOpts_upgrade :: !(Maybe Bool)
    , _installOpts_upgradeStrategy :: !(Maybe Text)
    , _installOpts_forceReinstall :: !(Maybe Bool)
    , _installOpts_ignoreInstalled :: !(Maybe Bool)
    , _installOpts_ignoreRequiresPython :: !(Maybe Bool)
    , _installOpts_noBuildIsolation :: !(Maybe Bool)
    , _installOpts_usePEP517 :: !(Maybe Bool)
    , _installOpts_noUsePEP517 :: !(Maybe Bool)
    , _installOpts_installOption :: !(Maybe Text)
    , _installOpts_globalOption :: !(Maybe Text)
    , _installOpts_compile :: !(Maybe Bool)
    , _installOpts_noCompile :: !(Maybe Bool)
    , _installOpts_noWarnScriptLocation :: !(Maybe Bool)
    , _installOpts_noWarnConflicts :: !(Maybe Bool)
    , _installOpts_noBinary :: !(Maybe FormatControl)
    , _installOpts_onlyBinary :: !(Maybe FormatControl)
    , _installOpts_preferBinary :: !(Maybe Bool)
    , _installOpts_noClean :: !(Maybe Bool)
    , _installOpts_requireHashes :: !(Maybe Bool)
    , _installOpts_progressBar :: !(Maybe ProgressBar)
    , _installOpts_index :: !(Maybe URL)
    , _installOpts_extraIndex :: !(Maybe URL)
    , _installOpts_noIndex :: !(Maybe Bool)
    , _installOpts_findLinks :: !(Maybe URL)
    } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance Default InstallOpts where
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
    fmtOpts (InstallOpts requirement
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
                         findLinks)
                           = noEmpty [ printOpt "requirement" requirement
                                     , printOpt "constraint" constraint
                                     , printOpt "no-deps" no-deps
                                     , printOpt "pre" pre
                                     , eitherPrint "editable" editable
                                     , printOpt "target" target
                                     , printOpt "platform" platform
                                     , printOpt "python-version" pythonVersion
                                     , printOpt "implementation" implementation
                                     , printOpt "abi" abi
                                     , printOpt "user" user
                                     , printOpt "root" root
                                     , printOpt "prefix" prefix
                                     , printOpt "build" build
                                     , printOpt "src" src
                                     , printOpt "upgrade" upgrade
                                     , upgrPrint upgradeStrategy
                                     , printOpt "force-reinstall" forceReinstall
                                     , printOpt "ignore-installed" ignoreInstalled
                                     , printOpt "ignore-requires-python" ignoreRequiresPython
                                     , printOpt "no-build-isolation" noBuildIsolation
                                     , printOpt "use-pep517" usePEP517
                                     , printOpt "no-use-pep517" noUsePEP517
                                     , printOpt "install-option" installOption
                                     , printOpt "global-option" globalOption
                                     , printOpt "compile" compile
                                     , printOpt "no-compile" noCompile
                                     , printOpt "no-warn-script-location" noWarnScriptLocation
                                     , printOpt "no-warn-conflicts" noWarnConflicts
                                     , controlPrint "no-binary" noBinary
                                     , controlPrint "only-binary" onlyBinary
                                     , printOpt "prefer-binary" preferBinary
                                     , printOpt "no-clean" noClean
                                     , printOpt "require-hashes" requireHashes
                                     , "--progress-bar" ++ progressPrint progressBar
                                     , printOpt "index-url" index
                                     , printOpt "extra-index-url" extraIndex
                                     , printOpt "no-index" noIndex
                                     , printOpt "find-links" findLinks
                                     ]
              upgrPrint Eager = "--upgrade-strategy eager"
              upgrPrint OnlyIfNeeded = "--upgrade-strategy only-if-needed"

data DownloadOpts = DownloadOpts
    { _downloadOpts_constraint :: !(Maybe FilePath)
    , _downloadOpts_requirement :: !(Maybe FilePath)
    , _downloadOpts_build :: !(Maybe FilePath)
    , _downloadOpts_noDeps :: !(Maybe Bool)
    , _downloadOpts_noBinary :: !(Maybe FormatControl)
    , _downloadOpts_onlyBinary :: !(Maybe FormatControl)
    , _downloadOpts_preferBinary :: !(Maybe Bool)
    , _downloadOpts_src :: !(Maybe FilePath)
    , _downloadOpts_pre :: !(Maybe Bool)
    , _downloadOpts_noClean :: !(Maybe Bool)
    , _downloadOpts_requireHashes :: !(Maybe Bool)
    , _downloadOpts_progressBar :: !(Maybe ProgressBar)
    , _downloadOpts_noBuildIsolation :: !(Maybe Bool)
    , _downloadOpts_usePEP517 :: !(Maybe Bool)
    , _downloadOpts_noUsePEP517 :: !(Maybe Bool)
    , _downloadOpts_dest :: !(Maybe FilePath)
    , _downloadOpts_platform :: !(Maybe Text)
    , _downloadOpts_pythonVersion :: !(Maybe Text)
    , _downloadOpts_implementation :: !(Maybe Text)
    , _downloadOpts_abi :: !(Maybe Text)
    , _downloadOpts_index :: !(Maybe URL)
    , _downloadOpts_extraIndex :: !(Maybe URL)
    , _downloadOpts_noIndex :: !(Maybe Bool)
    , _downloadOpts_findLinks :: !(Maybe URL)
    } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance Default DownloadOpts where
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

instance Opts DownloadOpts where
    fmtOpts (DownloadOpts constraint
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
                          findLinks)
                            = noEmpty [ printOpts "constraint" constraint
                                      , printOpts "requirement" requirement 
                                      , printOpts "build" build
                                      , printOpts "no-deps" noDeps
                                      , controlPrint "no-binary" noBinary
                                      , controlPrint "only-binary" onlyBinary
                                      , printOps "prefer-binary" preferBinary
                                      , printOpts "src" src
                                      , printOpts "pre" pre
                                      , printOpts "no-clean" noClean
                                      , printOpts "require-hashes" requireHashes
                                      , progressPrint progressBar
                                      , printOpts "no-build-isolation" noBuildIsolation
                                      , printOpts "use-pep517" usePEP517
                                      , printOpts "no-use-pep517" noUsePEP517
                                      , printOpts "dest" dest 
                                      , printOpts "platform" platform
                                      , printOpts "python-version" pythonVersion
                                      , printOpts "implementation" implementation
                                      , printOpts "abi" abi
                                      , printOpts "index" index
                                      , printOpts "extra-index" extraIndex
                                      , printOpts "no-index" noIndex
                                      , printOpts "find-links" findLinks
                                      ]

data UninstallOpts = UninstallOpts
    { _uninstallOpts_requirement :: !(Maybe FilePath)
    , _uninstallOpts_yes :: !(Maybe Bool)
    } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance Default UninstallOpts where
    def = Nothing Nothing

instance Opts DownloadOpts where
    fmtOpts (UninstallOpts requirement yes)
      = noEmpty [printOpts "requirement" requirement, printOpts "yes" yes]

data FreezeOpts = FreezeOpts
    { _freezeOpts_requirements :: !(Maybe FilePath)
    , _freezeOpts_findLinks :: !(Maybe URL)
    , _freezeOpts_local :: !(Maybe Bool)
    , _freezeOpts_user :: !(Maybe Bool)
    , _freezeOpts_path :: !(Maybe FilePath)
    , _freezeOpts_all :: !(Maybe Bool)
    , _freezeOpts_excludeEditable :: !(Maybe Bool)
    } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance Default FreezeOpts where
    def = Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing

instance Opts FreezeOpts where
    fmtOpts (FreezeOpts requirements
                        findLinks
                        local
                        user
                        path
                        all
                        excludeEditable)
                          = noEmpty [ printOpt "requirements" requirements
                                    , printOpt "find-links" findLinks
                                    , printOpt "local" local
                                    , printOpt "user" user
                                    , printOpt "path" path
                                    , printOpt "all" all
                                    , printOpt "exclude-editable" excludeEditable
                                    ]

data Format = Columns
            | Freeze
            | JSON
            deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

data ListOpts = ListOptions
    { _listOpts_outdated :: !(Maybe Bool)
    , _listOpts_uptodate :: !(Maybe Bool)
    , _listOpts_editable :: !(Maybe Bool)
    , _listOpts_local :: !(Maybe Bool)
    , _listOpts_user :: !(Maybe Bool)
    , _listOpts_path :: !(Maybe FilePath)
    , _listOpts_pre :: !(Maybe Bool)
    , _listOpts_format :: !(Maybe Format)
    , _listOpts_notRequired :: !(Maybe Bool)
    , _listOpts_excludeEditable :: !(Maybe Bool)
    , _listOpts_includeEditable :: !(Maybe Bool)
    , _listOpts_index :: !(Maybe URL)
    , _listOpts_extraIndex :: !(Maybe URL)
    , _listOpts_noIndex :: !(Maybe Bool)
    , _listOpts_findLinks :: !(Maybe URL)
    } deriving (Read, Eq, Ord, Data, Typeable, Generic)

instance Default ListOpts where
    def = ListOpts Nothing 
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
    fmtOpts (ListOpts outdated
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
                      findLinks)
                        = noEmpty [ printOpt "outdated" outdated
                                  , printOpt "uptodate" uptodate
                                  , printOpt "editable" editable
                                  , printOpt "local" local
                                  , printOpt "user" user
                                  , printOpt "path" path
                                  , printOpt "pre" pre
                                  , printOpt "format" format
                                  , printOpt "not-required" notRequired
                                  , printOpt "exclude-editable" excludeEditable
                                  , printOpt "include-editable" includeEditable
                                  , printOpt "index-url" index
                                  , printOpt "extra-index-url" extraIndex
                                  , printOpt "no-index" noIndex
                                  , printOpt "find-links" findLinks
                                  ]

newtype ShowOpts = ShowOpts 
    { _showOpts_files :: !(Maybe Bool) }
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

instance Default ShowOpts where
    def = ShowOpts Nothing

instance Opts ShowOpts where
    fmtOpts = noEmpty [printOpt "files" files]

newtype SearchOpts = SearchOpts
    { _searchOpts_index :: !(Maybe URL) }
    deriving (Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

instance Default SearchOpts where
    def = SearchOpts Nothing

instance Opts SearchOpts where
    fmtOpts (SearchOpts index) = noEmpty [printOpt "index" index]

newtype CheckOpts = CheckOpts ()
    deriving (Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

instance Default CheckOpts where
    def = CheckOpts ()

instance Opts CheckOpts where
    fmtOpts _ = ""

data ConfigOpts = ConfigOpts
    { _configOpts_editor :: !(Maybe Text)
    , _configOpts_global :: !(Maybe Bool)
    , _configOpts_user :: !(Maybe Bool)
    , _configOpts_site :: !(Maybe Bool)
    } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance Default ConfigOpts where
    def = ConfigOpts Nothing 
                     Nothing 
                     Nothing 
                     Nothing

instance Opts ConfigOpts where
    fmtOpts (ConfigOpts editor
                        global
                        user
                        site)
                          = noEmpty [ printOpt "editor" editor
                                    , printOpt "global" global
                                    , printOpt "user" user
                                    , printOpt "site" site
                                    ]

data WheelOpts = WheelOpts
    { _wheelOpts_wheelDir :: !(Maybe FilePath)
    , _wheelOpts_noBinary :: !(Maybe FormatControl)
    , _wheelOpts_onlyBinary :: !(Maybe FormatControl)
    , _wheelOpts_preferBinary :: !(Maybe Bool)
    , _wheelOpts_buildOption :: !(Maybe Text)
    , _wheelOpts_noBuildIsolation :: !(Maybe Bool)
    , _wheelOpts_usePEP517 :: !(Maybe Bool)
    , _wheelOpts_noUsePEP517 :: !(Maybe Bool)
    , _wheelOpts_constraint :: !(Maybe FilePath)
    , _wheelOpts_editable :: !(Maybe (Either FilePath URL))
    , _wheelOpts_requirement :: !(Maybe FilePath)
    , _wheelOpts_src :: !(Maybe FilePath)
    , _wheelOpts_ignoreRequiresPython :: !(Maybe Bool)
    , _wheelOpts_noDeps :: !(Maybe Bool)
    , _wheelOpts_build :: !(Maybe FilePath)
    , _wheelOpts_progressBar :: !(Maybe ProgressBar)
    , _wheelOpts_globalOption :: !(Maybe Text)
    , _wheelOpts_pre :: !(Maybe Bool)
    , _wheelOpts_noClean :: !(Maybe Bool)
    , _wheelOpts_requireHashes :: !(Maybe Bool)
    , _wheelOpts_index :: !(Maybe URL)
    , _wheelOpts_extraIndex :: !(Maybe URL)
    , _wheelOpts_noIndex :: !(Maybe Bool)
    , _wheelOpts_findLinks :: !(Maybe URL)
    } deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

instance Default WheelOpts where
    def = WheelOpts Nothing
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
    fmtOpts (WheelOpts wheelDir
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
                       findLinks)
                         = noEmpty [ printOpt "wheel-dir" wheelDir
                                   , controlPrint "no-binary" noBinary
                                   , controlPrint "only-binary" onlyBinary
                                   , printOpt "prefer-binary" preferBinary
                                   , printOpt "build-option" buildOption
                                   , printOpt "no-build-isolation" noBuildIsolation
                                   , printOpt "use-pep517" usePEP517
                                   , printOpt "no-use-pep517" noUsePEP517
                                   , printOpt "constraint" constraint
                                   , eitherPrint "editable" editable
                                   , printOpt "requirement" requirement
                                   , printOpt "src" src
                                   , printOpt "ignore-requires-python" ignoreRequiresPython
                                   , printOpt "no-deps" noDeps
                                   , printOpt "build" build
                                   , progressPrint progressBar
                                   , printOpt "global-option" globalOption
                                   , printOpt "pre" pre
                                   , printOpt "no-clean" noClean
                                   , printOpt "require-hashes" requireHashes
                                   , printOpt "index" index
                                   , printOpt "extra-index" extraIndex
                                   , printOpt "no-index" noIndex
                                   , printOpt "find-links" findLinks
                                   ]

data HashOpts = SHA256
              | SHA384
              | SHA512
              deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

instance Default HashOpts where
    def = SHA256

instance Opts HashOpts where
    fmtOpts opt = [printOpt "algorithm" (algName opt)]
        where algName SHA256 = "sha256"
              algName SHA384 = "sha384"
              algName SHA512 = "sha512"

data DebugOpts = DebugOpts
    { _debugOpts_platform :: !(Maybe Text)
    , _debugOpts_pythonVersion :: !(Maybe Text)
    , _debugOpts_implementation :: !(Maybe Text)
    , _debugOpts_abi :: !(Maybe Text)
    } deriving (Read, Eq, Ord, Data, Typeable, Generic)

instance Default DebugOpts where
    def = Nothing Nothing Nothing Nothing

instance Opt DebugOpts where
    fmtOpts (DebugOpts plat vers impl abi) = 
        noEmpty [ printOpt "platform" plat 
                , printOpt "python-version" vers 
                , printOpt "implementation" impl 
                , printOpt "abi" abi
                ]

install = undefined

installReqSpec :: Options
               -> ReqSpec
               -> PkgIndexOpts
               -> IO Text
installReqSpec = undefined

installReqFile :: Options
               -> FilePath
               -> PkgIndexOpts
               -> IO Text
installReqFile = undefined

installFromURL :: Options
               -> URL
               -> IO Text
installFromURL = undefined

installFromPath :: Options
                -> FilePath
                -> IO Text
installFromPath = undefined


download = undefined

downloadReqSpec = undefined
downloadReqSpec :: Options
                -> [(ReqSpec, PkgIndexOpts)]
                -> IO Text

downloadReqFile = undefined
downloadReqFile :: Options
                -> [(FilePath, PkgIndexOpts)]
                -> IO Text

downloadFromURL :: Options
                -> [URL]
                -> IO Text
downloadFromURL = undefined

downloadFromPath :: Options
                 -> FilePath
                 -> IO Text
downloadFromPath = undefined

uninstall = undefined

uninstallPkg :: Options
             -> [Text]
             -> IO Text
uninstallPkg = undefined

uninstallReqFile :: Options
                 -> [FilePath]
                 -> IO Text
uninstallReqFile = undefined

freeze :: Options -> IO Text
freeze opts = pip $ "freeze" : fmtOpts opts

list :: Options -> IO Text
list opts = pip $ "list" : fmtOpts opts

show :: Options
     -> [Text]
     -> IO Text
show = undefined

search :: Options
       -> Text
       -> IO Text
search opts pkg = pip $ "search" : fmtOpts opts : pkg

check :: CheckOpts -> IO Text
check opts = pip $ "check" : fmtOpts opts

config = undefined

configGet :: FileOption
          -> Text
          -> IO Text
configGet = undefined

configList :: FileOption
           -> IO Text
configList = undefined

configEdit :: FileOption
           -> Maybe FilePath
           -> IO Text
configEdit = undefined

configSet :: FileOption
          -> Text
          -> Text
          -> IO Text
configSet = undefined

configUnset :: FileOption 
            -> Text
            -> IO Text
configUnset = undefined

wheel = undefined
downloadreqspec :: options
                -> [(reqspec, pkgindexopts)]
                -> io text
downloadreqspec = undefined

wheel = undefined

wheelReqSpec :: Options
             -> ReqSpec
             -> PkgIndexOpts
             -> IO Text
wheelReqSpec = undefined

wheelReqFile :: Options
             -> [(FilePath, PkgIndexOpts)]
             -> IO Text
wheelReqFile = undefined

wheelFromURL :: Options
             -> [URL]
             -> IO Text
wheelFromURL = undefined

wheelFromPath :: Options
              -> FilePath
              -> IO Text
wheelFromPath = undefined

hash :: HashOpts
     -> [FilePath]
     -> IO Text
hash fp opts = pip $ "hash" : fmtOpts opts : fp 

debug :: DebugOpts -> IO Text
debug opts = pip $ "debug" : fmtOpts opts

printOpt :: Text 
         -> Maybe Text 
         -> Text
printOpt _ Nothing = ""
printOpt optName optVar = "--" ++ optName ++ " " ++ optVar

printOpt :: Text
         -> Maybe Bool
         -> Text
printOpt _ Nothing = ""
printOpt _ (Just False) = ""
printOpt optName (Just True) = "--" ++ optName

controlPrint :: Text 
             -> Maybe FormatControl
             -> Text
controlPrint _ Nothing
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

pip :: [Text] -> IO Text
pip = "pip" $|
