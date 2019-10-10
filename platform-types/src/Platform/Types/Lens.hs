{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
module Platform.Types.Lens where

import "lens" Control.Lens
import        Platform.Types.Types

makeClassy ''ContainerID
makeClassy ''QueueID

