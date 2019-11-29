{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

module Platform.Packaging.PythonImports.InspectPackages where

import "text" Data.Text (pack)
import "data-default" Data.Default (def)
import "lens" Control.Lens
import Platform.Packaging.Pip

makeLenses ''DownloadOpts

inspectOpts :: Text -> DownloadOpts
inspectOpts dir = build . bin . deps $ def
    where build = set downloadOpts_build dir
          bin = set downloadOpts_noBinary All
          deps = set downloadOpts_noDeps True

downloadPkg :: (MonadThrow m, MonadIO m, MonadMask m)
            => FilePath 
            -> Text 
            -> m ()
downloadPkg dir pkg = do
    download def (inspectOps $ pack dir) pkg
    return ()
