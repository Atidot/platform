{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Atidot.Platform.Deployment where

import "base"                   Data.Typeable
import "free"                   Control.Monad.Free
import "free"                   Control.Monad.Free.TH
import "text"                   Data.Text(Text)

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


    deriving (Typeable, Functor)

type DeploymentM = Free Deployment

makeFree ''Deployment

placeHolderContainer :: Name
placeHolderContainer = "helloWorld"

placeHolderSecret :: SecretName
placeHolderSecret = "tutorials/MyFirstTutorialSecret" -- this must exist in aws secrets manager in the user's account

placeHolderData :: DiskName
placeHolderData = "data"

hello :: DeploymentM ()
hello = do
    _ <- container placeHolderContainer
    return ()

nsss :: DeploymentM ()
nsss = do
    s <- secret placeHolderSecret         -- declares secret that already exists in aws secrets manager
    dir <- mount placeHolderData          -- declares the mounting of volume data into the machine
    c <- container placeHolderContainer   -- declares the container running hello world
    attachSecret s c                      -- attaches secret to the container
    attachVolume dir c                    -- attaches the volume to the container also
    execute [] c []                       -- executes the program inside the container


kiss :: DeploymentM ()
kiss = do
    _dbUrl <- secret placeHolderSecret
    _volume1 <- mount placeHolderData
    c <- container placeHolderContainer
    execute [] c []

noneExistentSecret :: DeploymentM ()
noneExistentSecret = do
    let noneExistantSecretPlaceholder = "some/secret"
    _ <- secret noneExistantSecretPlaceholder
    return ()

noneExistentSecretAttached :: DeploymentM ()
noneExistentSecretAttached = do
    let noneExistantSecretPlaceholder = "some/secret"
    attachSecret noneExistantSecretPlaceholder placeHolderContainer
    return ()

secretDeclaredTwice :: DeploymentM ()
secretDeclaredTwice = do
    _ <- secret placeHolderSecret
    _ <- secret placeHolderSecret
    return ()

secretAttachedTwice :: DeploymentM ()
secretAttachedTwice = do
    s <- secret placeHolderSecret
    c <- container placeHolderContainer
    attachSecret s c
    attachSecret s c

containerDoesNotExists :: DeploymentM ()
containerDoesNotExists = do
    s <- secret placeHolderSecret
    attachSecret s placeHolderContainer
