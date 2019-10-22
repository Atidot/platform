{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Platform.Types.Default where

import "data-default" Data.Default
import Platform.Types.Types

instance Default ContainerID where
  def = ContainerID ""

instance Default QueueID where
  def = QueueID ""

instance Default DatabaseID where
  def = DatabaseID ""

instance Default FileID where
  def = FileID ""
