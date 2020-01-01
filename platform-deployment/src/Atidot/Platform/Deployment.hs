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
    = Container Name (Bool -> a) -- bool
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

hello :: DeploymentM Bool
hello =
    container "hello-world"

nsss :: DeploymentM Bool
nsss = do
    s <- secret "tutorials/MyFirstTutorialSecret"     -- declares secret that already exists in aws secrets manager
    dir <- mount "data"                               -- declares the mounting of volume data into the machine
    b <- container "hello-world"                      -- declares the container running hello world
    attachSecret s "hello-world"                      -- attaches secret to the container
    attachVolume dir "hello-world"                    -- attaches the volume to the container also
    execute [] "hello-world" []                       -- executes the program inside the container
    return b


kiss :: DeploymentM Bool
kiss = do
    _dbUrl <- secret "tutorials/MyFirstTutorialSecret"
    _volume1 <- mount "data"
    b <- container "hello-world"
    execute [] "hello-world" []
    return b

noneExistentSecret :: DeploymentM Bool
noneExistentSecret = do
    _ <- secret "tutorials/MyFirstTutorialSecret2"
    return False

noneExistentSecretAttached :: DeploymentM Bool
noneExistentSecretAttached = do
    attachSecret "tutorials/MyFirstTutorialSecret2" "hello-world"
    return False

secretDeclaredTwice :: DeploymentM Bool
secretDeclaredTwice = do
    _ <- secret "tutorials/MyFirstTutorialSecret"
    _ <- secret "tutorials/MyFirstTutorialSecret"
    return False

secretAttachedTwice :: DeploymentM Bool
secretAttachedTwice = do
    s <- secret "tutorials/MyFirstTutorialSecret"
    _ <- container "hello-world"
    attachSecret s "hello-world"
    attachSecret s "hello-world"
    return False

containerDoesNotExists :: DeploymentM Bool
containerDoesNotExists = do
    s <- secret "tutorials/MyFirstTutorialSecret"
    attachSecret s "hello-world"
    attachSecret s "hello-world"
    return False