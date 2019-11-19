{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Platform.Packaging.Pip where

import "data-text" Data.Text (Text)
import "shellmet"  Shellmet (($|))

install = undefined

class Opts a where
    fmtOpts :: a -> [Text]

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

data FreezeOpts = FreezeOpts
    { _freezeOpts_requirements :: !(Maybe FilePath)
    , _freezeOpts_findLinks :: !(Maybe URL)
    , _freezeOpts_local :: !(Maybe Bool)
    , _freezeOpts_user :: !(Maybe Bool)
    , _freezeOpts_path :: !(Maybe FilePath)
    , _freezeOpts_all :: !(Maybe Bool)
    , _freezeOpts_excludeEditable :: !(Maybe Bool)
    } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance Opts FreezeOpts where
    fmtOpts (FreezeOpts requirements
                        findLinks
                        local
                        user
                        path
                        all
                        excludeEditable)
                          = notEmpty $ [ printOpt "--requirements" requirements
                                       , printOpt "--find-links" findLinks
                                       , printOpt "--local" local
                                       , printOpt "--user" user
                                       , printOpt "--path" path
                                       , printOpt "--all" all
                                       , printOpt "--exclude-editable" excludeEditable
                                       ]

freeze :: Options -> IO Text
freeze opts = pip $ "freeze" : fmtOpts opts

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
                        = noEmpty [ printOpt "--outdated" outdated
                                  , printOpt "--uptodate" uptodate
                                  , printOpt "--editable" editable
                                  , printOpt "--local" local
                                  , printOpt "--user" user
                                  , printOpt "--path" path
                                  , printOpt "--pre" pre
                                  , printOpt "--format" format
                                  , printOpt "--not-required" notRequired
                                  , printOpt "--exclude-editable" excludeEditable
                                  , printOpt "--include-editable" includeEditable
                                  , printOpt "--index-url" index
                                  , printOpt "--extra-index-url" extraIndex
                                  , printOpt "--no-index" noIndex
                                  , printOpt "--find-links" findLinks
                                  ]

list :: Options -> IO Text
list opts = pip $ "list" : fmtOpts opts

show :: Options
     -> [Text]
     -> IO Text
show = undefined

newtype SearchOpts = SearchOpts
    { _searchOpts_index :: !(Maybe URL) }
    deriving (Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

instance Default SearchOpts where
    def = Nothing

instance Opts SearchOpts
    fmtOpts (SearchOpts index) = noEmpty [printOpt "--index" index]

search :: Options
       -> Text
       -> IO Text
search opts pkg = pip $ "search" : fmtOpts opts : pkg

newtype CheckOpts = CheckOpts ()
    deriving (Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

instance Default CheckOpts where
    def = CheckOpts ()

instance Opts CheckOpts where
    fmtOpts _ = ""

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

data HashOpts = SHA256
              | SHA384
              | SHA512
              deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

instance Default HashOpts where
    def = SHA256

fmtOpts opt = [printOpt "--algorithm" (algName opt)]
    where algName SHA256 = "sha256"
          algName SHA384 = "sha384"
          algName SHA512 = "sha512"

hash :: HashOpts
     -> [FilePath]
     -> IO Text
hash fp opts = pip $ "hash" : fmtOpts opts : fp 

data DebugOpts = DebugOpts
    { _debugOpts_platform :: !(Maybe Text)
    , _debugOpts_pythonVersion :: !(Maybe Text)
    , _debugOpts_implementation :: !(Maybe Text)
    , _debugOpts_abi :: !(Maybe Text)
    } deriving (Read, Eq, Ord, Data, Typeable, Generic)

instance Default DebugOpts where
    def = Nothing Nothing Nothing Nothing

fmtOpts (DebugOpts plat vers impl abi) = 
    noEmpty [ printOpt "--platform" plat 
            , printOpt "--python-version" vers 
            , printOpt "--implementation" impl 
            , printOpt "--abi" abi
            ]

debug :: DebugOpts -> IO Text
debug opts = pip $ "debug" : fmtOpts opts

printOpt :: Text 
         -> Maybe Text 
         -> Text
printOpt _ Nothing = ""
printOpt optName optVar = optName ++ " " ++ optVar

printOpt :: Text
         -> Maybe Bool
         -> Text
printOpt _ Nothing = ""
printOpt _ (Just False) = ""
printOpt optName (Just True) = optName

noEmpty :: [Text] -> [Text]
noEmpty = filter (/= "")

pip :: [Text] -> IO Text
pip = "pip" $|
