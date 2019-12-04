{-# LANGUAGE QuasiQuotes #-}
module Atidot.Platform.Deployment.Interpreter.AMI where

import "text" Data.Text (Text)
import qualified "text" Data.Text as T
import qualified "text" Data.Text.IO as T
import "free"           Control.Monad.Free
import                  Atidot.Platform.Deployment
import "mtl"           Control.Monad.Writer (Writer)
import "mtl"            Control.Monad.State
import "mtl"            Control.Monad.Identity (Identity(..))

import "ginger"         Text.Ginger
import "raw-strings-qq" Text.RawString.QQ
import "exceptions"     Control.Monad.Catch (MonadMask, bracket)

data AMIConfig =
    AMIConfig

nullResolver :: IncludeResolver Identity
nullResolver = const $ return Nothing

toTemplate :: String -> Template SourcePos
toTemplate template = either (error . show) id . runIdentity $
    parseGinger nullResolver Nothing template

renderProvider :: T.Text
renderProvider = do
    let ctx :: GVal (Run SourcePos (Writer Text) Text)
        ctx = dict
            [ (("region" :: Text) ~> ("bobo" :: Text))
            , (("profile" :: Text) ~> (5 :: Int))
            ]
    easyRender ctx $ toTemplate provider

provider :: String
provider = [r|
provider "aws" {
    region = "{{region}}"
    profile = "{{profile}}"
}
        |]

runAMI :: AMIConfig -> DeploymentM a -> IO ()
runAMI config dep = do
    _ <-  (runStateT (iterM run dep) config)
    return ()
    -- bracket init'
    --         fini
    --         body
    where
        -- init' = return ()
        -- fini = return ()
        -- body _ = do
        --     _ <-  (runStateT (iterM run dep) config)
        --     return ()

        run :: Deployment (StateT AMIConfig IO a) -> StateT AMIConfig IO a
        run (Container containerName next) = do
            lift $ T.putStrLn renderProvider
            liftIO $ putStrLn "some container cmd"
            next True
        run (Secret secretData next) = do
            liftIO $ putStrLn "some secret thingy"
            next ""
        run (Mount disk volume next) = do
            liftIO $ putStrLn "some storage mount"
            next True