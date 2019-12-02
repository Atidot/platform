{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Atidot.Platform.Deployment where

import "base"                   Control.Monad(void)
import "base"                   Data.Traversable(for)
import "base"                   Data.Monoid
import "base"                   Data.Typeable
import "free"                   Control.Monad.Free
import "free"                   Control.Monad.Free.TH

-- import qualified "containers"   Data.Map as M
-- import qualified "text"         Data.Text as T

someFunc = putStrLn "hello"

data Docker = DockerC FilePath
data Volume = Volume FilePath
data Disk = Disk FilePath

data Deployment a
    = Docker Docker (Bool -> a) -- bool
    | Secret FilePath (FilePath -> a)
    | Config FilePath (FilePath -> a)
    | Mount Disk Volume (Bool -> a)
    | Start           ([String] -> a)
    deriving (Typeable, Functor)

type DeploymentM = Free Deployment
data TestConfig = TestConfig

makeFree ''Deployment

pacificLife :: DeploymentM Bool
pacificLife = do
    dbUrl  <- secret "DB_URL"
    dbPass <- secret "DB_PASSWORD"
    volume1 <- mount (Disk "bli/bloo") (Volume "bli/bloo")
    volume2 <- mount (Disk "bli/bliiii/oooo") (Volume "bli/bli")
    b <- docker $ DockerC "atidot/undins"
    return b




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
