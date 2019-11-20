{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Platform.Packaging.Pip 
    (install, download, uninstall, freeze, list, show, search, check, config, wheel, hash, debug, UpgradeStrategy, FormatControl, ProgressBar, InstallOpts, DownloadOpts, UninstallOpts, FreezeOpts, ListOpts, ShowOpts, SearchOpts, CheckOpts, ConfigOpts, WheelOpts, HashOpts, DebugOpts, PipInput, ConfigInput) where

import "base" Data.Data (Data)
import "data-default" Data.Default (Default, def)
import "text" Data.Text (Text)
import qualified "text" Data.Text as T
import "base" Data.Typeable (Typeable)
import "base" GHC.Generics (Generic)
import Platform.Packaging.Pip.Types
import "shellmet" Shellmet (($|))

-- Do not export this, as it's not very type-safe.
-- It exists because several of the commands have identical
-- inputs, so their processing can happen all at once.
pipCmd ::
  (Opts a) =>
  Text ->
  a ->
  PipInput ->
  IO Text
pipCmd command input opts =
  pip $ command : fmtOpts opts ++ fmtInput input

install ::
  InstallOpts ->
  PipInput ->
  IO Text
install = pipCmd "install" 

download ::
  DownloadOpts ->
  PipInput ->
  IO Text
download = pipCmd "download"

uninstall ::
  UninstallOpts ->
  UninstallInput ->
  IO Text
uninstall = pipCmd "uninstall" 

freeze :: FreezeOpts -> IO Text
freeze opts = pip $ "freeze" : fmtOpts opts

list :: ListOpts -> IO Text
list opts = pip $ "list" : fmtOpts opts

show ::
  ShowOpts ->
  PkgList ->
  IO Text
show opts pkgs = pip $ "show" : fmtOpts opts ++ pkgs

search ::
  SearchOpts ->
  Text ->
  IO Text
search opts pkg = pip $ "search" : fmtOpts opts ++ [pkg]

check :: CheckOpts -> IO Text
check opts = pip $ "check" : fmtOpts opts

config ::
  ConfigOpts ->
  ConfigInput ->
  IO Text
config = pipCmd "config"

wheel ::
  WheelOpts ->
  PipInput ->
  IO Text
wheel = pipCmd "wheel"

hash ::
  HashOpts ->
  FileList ->
  IO Text
hash opts fp = pip $ "hash" : fmtOpts opts : fp

debug :: DebugOpts -> IO Text
debug opts = pip $ "debug" : fmtOpts opts
