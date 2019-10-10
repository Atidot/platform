{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module Platform.Types.Default where

import "data-default" Data.Default
import                Platform.Types.Types


instance Default ContainerID where
    def = ContainerID ""

instance Default QueueID where
    def = QueueID ""


