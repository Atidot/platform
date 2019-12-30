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

kiss :: DeploymentM Bool
kiss = do
    _dbUrl  <- secret "tutorials/MyFirstTutorialSecret"
    _volume1 <- mount "data"
    b <- container "hello-world"
    execute [] "hello-world" []
    return b


nsss :: DeploymentM Bool
nsss = do
    s <- secret "tutorials/MyFirstTutorialSecret"
    dir <- mount "data"
    b <- container "hello-world"
    attachSecret s "hello-world"
    attachVolume dir "hello-world"
    execute [] "hello-world" []
    return b

pacificLife :: DeploymentM Bool
pacificLife = do
    _dbUrl  <- secret "DB_URL"
    _dbPass <- secret "DB_PASSWORD"
    _volume1 <- mount "bli/bloo"
    _volume2 <- mount "bli/bliiii/oooo"
    _b <- container "atidot/undins"
    b2 <- container "atidot/undins2"
    return b2




-- Good for AMI, ECS, Kubernetes, Test, everything
-- newtype Deployment = Deployment
-- newtype ResourceName = ResourceName
-- newtype Secret = Secret


-- portIn :: Int -> Deployment ()
-- portIn n = undefined

    -- make it available and ready
-- docker :: DockerImage -> Deployment Bool
-- docker image = undefined
        -- 1. fetch it (docker load in AMI, or setting up Registry for ECS)
        -- 2. it runs on boots - autostats

-- secret :: Key -> Deployment Secret
-- secret _ = undefined
        -- 1. AWS Secret Store
        -- 2. Vault Hashicorp
        -- 3. Env variable

-- config :: Config -> Deployemt Config
-- config _ = undefined
        --

-- mount :: S3Bucket -> Volume -> Deployment Bool
-- mount _ = undefined
        -- in runTest -- could be mapping of a JSON-like tree structure
        -- in AMI     -- S3 bucket
        -- in ECS     -- ^

-- file :: File -> Deployment ()
-- file _ = undefined

    -- Less of these:
    -- run :: Deployment ()
    -- autostart :: Deployment ()



    ---------------------------------------------
-- runAMI :: AMIConfig -> Platform m -> IO ()
-- runAMI _ _ = undefined

-- runTest :: TestConfig -> Platform m -> IO ()
-- runTest _ _ = undefined
--     --

-- runECS :: ECSConfig -> Platform m -> IO ()
-- runECS _ _ = undefined

-- runKube :: KubeConfig -> Platform m -> IO ()
-- runKube _ _ = undefined
