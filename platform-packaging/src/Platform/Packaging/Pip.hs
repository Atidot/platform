{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Platform.Packaging.Pip 
    (install, download, uninstall, freeze, list, pipShow, search, check, config, wheel, hash, debug, UpgradeStrategy, FormatControl, OutputFormat, ProgressBar, HashAlgs, InstallOpts, DownloadOpts, UninstallOpts, FreezeOpts, ListOpts, ShowOpts, SearchOpts, CheckOpts, ConfigOpts, WheelOpts, HashOpts, DebugOpts, PipInput, UninstallInput, ConfigInput) where

import           "base"            Data.Data (Data)
import           "data-default"    Data.Default (Default, def)
import           "text"            Data.Text (Text)
import qualified "text"            Data.Text as T
import           "base"            Data.Typeable (Typeable)
import           "base"            GHC.Generics (Generic)
import           "regex-tdfa"      Text.Regex.TDFA
import           "shellmet"        Shellmet (($|))
import                             Platform.Packaging.Pip.Types

-- Do not export this, as it allows e.g. InstallOpts to be used with "wheel".
-- It exists because several of the commands have identical
-- inputs, so their processing can happen all at once.
pipCmd :: (Opts a) 
       => Text 
       -> GeneralOpts 
       -> a 
       -> PipInput 
       -> IO Text
pipCmd command gOpts opts input =
  pip $ command : fmtOpts gOpts ++ fmtOpts opts ++ fmtInput input

install :: GeneralOpts 
        -> InstallOpts 
        -> PipInput 
        -> IO Text
install = pipCmd "install" 

download :: GeneralOpts 
         -> DownloadOpts 
         -> PipInput 
         -> IO Text
download = pipCmd "download"

uninstall :: GeneralOpts 
          -> UninstallOpts 
          -> UninstallInput 
          -> IO Text
uninstall gOpts opts input 
  = pip $ "uninstall" : fmtOpts gOpts ++ fmtOpts opts ++ fmtInput input

freeze :: GeneralOpts 
       -> FreezeOpts 
       -> IO Text
freeze gOpts opts = pip $ "freeze" : fmtOpts gOpts ++ fmtOpts opts

list :: GeneralOpts 
     -> ListOpts 
     -> IO Text
list gOpts opts = pip $ "list" : fmtOpts gOpts ++ fmtOpts opts

pipShow :: GeneralOpts 
        -> ShowOpts 
        -> PkgList 
        -> IO Text
pipShow gOpts opts pkgs = pip $ "show" : fmtOpts gOpts ++ fmtOpts opts ++ pkgs

search :: GeneralOpts 
       -> SearchOpts 
       -> Text 
       -> IO Text
search gOpts opts pkg = pip $ "search" : fmtOpts gOpts ++ fmtOpts opts ++ [pkg]

check :: GeneralOpts 
      -> CheckOpts 
      -> IO Text
check gOpts opts = pip $ "check" : fmtOpts gOpts ++ fmtOpts opts

config :: GeneralOpts 
       -> ConfigOpts 
       -> ConfigInput 
       -> IO Text
config gOpts opts input = pip $ "config" : fmtOpts gOpts ++ fmtOpts opts ++ fmtInput input

wheel :: GeneralOpts -> WheelOpts -> PipInput -> IO Text
wheel = pipCmd "wheel"

hash :: GeneralOpts 
     -> HashOpts 
     -> FileList 
     -> IO Text
hash gOpts opts fps = pip $ "hash" : fmtOpts gOpts ++ fmtOpts opts ++ fps

debug :: GeneralOpts 
      -> DebugOpts 
      -> IO Text
debug gOpts opts = pip $ "debug" : fmtOpts gOpts ++ fmtOpts opts

pip :: [Text] -> IO Text
pip = ($|) "pip" 
