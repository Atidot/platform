{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Atidot.Platform.Deployment where

import "base"                     Data.Typeable
import "free"                     Control.Monad.Free
import "free"                     Control.Monad.Free.TH
import "text"                     Data.Text(Text)
import "platform-packaging-types" Platform.Packaging.Types (ContainerEnv)

data Volume = Volume FilePath
data Disk = Disk FilePath

type Name = Text
type SecretName = Text
type DiskName = Text
type VolumeName = Text
type FolderDir = Text
type Arg = Text

data Deployment a
    -- resource declarations
    = Container Name (Name -> a) -- bool
    | Secret SecretName (SecretName -> a)
    | Mount DiskName (VolumeName -> a)
    -- resource attachments
    | AttachSecret SecretName Name a
    | AttachVolume FolderDir Name a
    -- execution
    | Execute [Arg] Name [Arg] a
    | MakeContainer ContainerEnv (Name -> a)
    deriving (Typeable, Functor)

type DeploymentM = Free Deployment

makeFree ''Deployment

placeHolderContainer :: Name
placeHolderContainer = "hello-world"

placeHolderSecret :: SecretName
placeHolderSecret = "tutorial/MyFirstTutorialSecret" -- This must exist in the AWS secrets manager

placeHolderData :: DiskName
placeHolderData = "data"

nsss :: DeploymentM ()
nsss = do
    s <- secret placeHolderSecret         -- declares secret that already exists in aws secrets manager
    dir <- mount placeHolderData          -- declares the mounting of volume data into the machine
    c <- container placeHolderContainer   -- declares the container running hello world
    attachSecret s c                      -- attaches secret to the container
    attachVolume dir c                    -- attaches the volume to the container also
    execute [] c []                       -- executes the program inside the container

nsss2 :: DeploymentM ()
nsss2 = do
    s <- secret placeHolderSecret             -- declares secret that already exists in aws secrets manager
    dir <- mount placeHolderData              -- declares the mounting of volume data into the machine
    c <- container "/home/user/docker.tar.gz" -- declares the container running hello world
    attachSecret s c                          -- attaches secret to the container
    attachVolume dir c                        -- attaches the volume to the container also

kiss :: DeploymentM ()
kiss = do
    _dbUrl <- secret placeHolderSecret
    _volume1 <- mount placeHolderData
    c <- container placeHolderContainer
    execute [] c []
