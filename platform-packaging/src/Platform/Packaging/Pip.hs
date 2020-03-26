{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Platform.Packaging.Pip
    (install, download, uninstall, freeze, list, pipShow, search, check, config, wheel, hash, debug, UpgradeStrategy, FormatControl, OutputFormat, ProgressBar, HashAlgs, InstallOpts, DownloadOpts, UninstallOpts, FreezeOpts, ListOpts, ShowOpts, SearchOpts, CheckOpts, ConfigOpts, WheelOpts, HashOpts, DebugOpts, PipInput, UninstallInput, ConfigInput) where

import           "base"                     Debug.Trace (trace)
import           "base"                     Data.Data (Data)
import           "data-default"             Data.Default (Default, def)
import           "text"                     Data.Text (Text)
import qualified "text"                     Data.Text as T
import           "base"                     Data.Typeable (Typeable)
import           "base"                     GHC.Generics (Generic)
import           "regex-pcre"               Text.Regex.PCRE
import           "shelly"                   Shelly hiding (trace)
import           "platform-packaging-types" Platform.Packaging.Pip.Types
import                                      Platform.Packaging.PythonImports.Internal.Download

-- Do not export this, as it allows e.g. InstallOpts to be used with "wheel".
-- It exists because several of the commands have identical
-- inputs, so their processing can happen all at once.

pip :: [Text] -> IO Text
pip args = shelly $ run (fromText "pip3") args

pipCmd :: (Opts a)
       => Text
       -> GeneralOpts
       -> a
       -> PipInput
       -> IO Text
pipCmd command gOpts opts input
  = pip
  $ command
  : fmtOpts gOpts ++ fmtOpts opts ++ fmtInput input

install :: GeneralOpts
        -> InstallOpts
        -> PipInput
        -> IO Text
install = pipCmd "install"

-- Search for
--     Found link https://files.pythonhosted.org/packages/2f/79/f236ab1cfde94bac03d7b58f3f2ab0b1cc71d6a8bda3b25ce370a9fe4ab1/pandas-1.0.3.tar.gz#sha256=32f42e322fb903d0e189a4c10b75ba70d90958cc4f66a1781ed027f1a1d14586 (from https://pypi.org/simple/pandas/) (requires-python:>=3.6.1), version: 1.0.3
-- And then interrupt and take over downloading.
download :: GeneralOpts
         -> DownloadOpts
         -> PipInput
         -> IO ()
download gOpts opts input
  = trace "trying to download"
  . shelly
  . interruptPipDownload
  $ fmtOpts gOpts ++ fmtOpts opts ++ fmtInput input

uninstall :: GeneralOpts
          -> UninstallOpts
          -> UninstallInput
          -> IO Text
uninstall gOpts opts input
  = pip
  $ "uninstall"
  : fmtOpts gOpts ++ fmtOpts opts ++ fmtInput input

freeze :: GeneralOpts
       -> FreezeOpts
       -> IO Text
freeze gOpts opts
  = pip
  $ "freeze"
  : fmtOpts gOpts ++ fmtOpts opts

list :: GeneralOpts
     -> ListOpts
     -> IO Text
list gOpts opts
  = pip
  $ "list"
  : fmtOpts gOpts ++ fmtOpts opts

pipShow :: GeneralOpts
        -> ShowOpts
        -> PkgList
        -> IO Text
pipShow gOpts opts pkgs
  = pip
  $ "show"
  : fmtOpts gOpts ++ fmtOpts opts ++ pkgs

search :: GeneralOpts
       -> SearchOpts
       -> Text
       -> IO Text
search gOpts opts pkg
  = pip
  $ "search"
  : fmtOpts gOpts ++ fmtOpts opts ++ [pkg]

check :: GeneralOpts
      -> CheckOpts
      -> IO Text
check gOpts opts
  = pip
  $ "check"
  : fmtOpts gOpts ++ fmtOpts opts

config :: GeneralOpts
       -> ConfigOpts
       -> ConfigInput
       -> IO Text
config gOpts opts input
  = pip
  $ "config"
  : fmtOpts gOpts ++ fmtOpts opts ++ fmtInput input

wheel :: GeneralOpts -> WheelOpts -> PipInput -> IO Text
wheel = pipCmd "wheel"

hash :: GeneralOpts
     -> HashOpts
     -> FileList
     -> IO Text
hash gOpts opts fps
  = pip
  $ "hash"
  : fmtOpts gOpts ++ fmtOpts opts ++ fps

debug :: GeneralOpts
      -> DebugOpts
      -> IO Text
debug gOpts opts
  = pip
  $ "debug"  : fmtOpts gOpts ++ fmtOpts opts
