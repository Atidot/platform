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
type SecretValue = String
type SecretName = Text
type DiskName = Text
type VolumeName = Text
type FolderDir = Text
type Arg = Text

data Deployment a
    -- resource declarations
    = Container Name (Name -> a) -- bool
    | Secret SecretValue (SecretName -> a)
    | Mount DiskName (VolumeName -> a)
    -- resource attachments
    | AttachSecret SecretName Name a
    | AttachVolume FolderDir Name a
    -- execution
    | Execute [Arg] Name [Arg] a


    deriving (Typeable, Functor)

type DeploymentM = Free Deployment

makeFree ''Deployment

hello :: DeploymentM ()
hello = do
    _ <- container "hello-world"
    return ()

nsss :: DeploymentM ()
nsss = do
    s <- secret "tutorials/MyFirstTutorialSecret"     -- declares secret that already exists in aws secrets manager
    dir <- mount "data"                               -- declares the mounting of volume data into the machine
    c <- container "hello-world"                      -- declares the container running hello world
    attachSecret s c                      -- attaches secret to the container
    attachVolume dir c                    -- attaches the volume to the container also
    execute [] c []                       -- executes the program inside the container


kiss :: DeploymentM ()
kiss = do
    _dbUrl <- secret "tutorials/MyFirstTutorialSecret"
    _volume1 <- mount "data"
    c <- container "hello-world"
    execute [] c []

noneExistentSecret :: DeploymentM ()
noneExistentSecret = do
    _ <- secret "tutorials/MyFirstTutorialSecret2"
    return ()

noneExistentSecretAttached :: DeploymentM ()
noneExistentSecretAttached = do
    attachSecret "tutorials/MyFirstTutorialSecret2" "hello-world"
    return ()

secretDeclaredTwice :: DeploymentM ()
secretDeclaredTwice = do
    _ <- secret "tutorials/MyFirstTutorialSecret"
    _ <- secret "tutorials/MyFirstTutorialSecret"
    return ()

secretAttachedTwice :: DeploymentM ()
secretAttachedTwice = do
    s <- secret "tutorials/MyFirstTutorialSecret"
    c <- container "hello-world"
    attachSecret s c
    attachSecret s c

containerDoesNotExists :: DeploymentM ()
containerDoesNotExists = do
    s <- secret "tutorials/MyFirstTutorialSecret"
    attachSecret s "hello-world"
    attachSecret s "hello-world"
