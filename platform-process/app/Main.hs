{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           "data-default"             Data.Default (def)
import           "dockerfile"               Data.Docker
import           "text"                     Data.Text (Text)
import qualified "text"                     Data.Text as T (pack, unpack)
import           "lens"                     Control.Lens
import           "exceptions"               Control.Monad.Catch (Exception, throwM, bracket)
import           "mtl"                      Control.Monad.State (execStateT)
import           "directory"                System.Directory (createDirectoryIfMissing, copyFile, getCurrentDirectory, setCurrentDirectory, canonicalizePath, removeFile)
import           "regex-tdfa"               Text.Regex.TDFA
import           "shelly"                   Shelly (Sh, shelly)
import qualified "shelly"                   Shelly as Sh (run)
import           "platform-dsl"             Platform.DSL
import           "platform-process"         Platform.Process
import           "platform-harness"         Platform.Harness
import           "platform-packaging"       Platform.Packaging
import           "platform-packaging-types" Platform.Packaging.Types

data NoHarnessException = NoHarnessException
    deriving Show

instance Exception NoHarnessException

main :: IO ()
main = do
    bracket init'
            body
            fini
    where
        -- This init' presumes that platform-process is being run from the /build subdirectory
        init' = do
            originalDir <- getCurrentDirectory
            let newDir = originalDir <> "/../testDockers"
            setCurrentDirectory newDir
            writeD "consumer" "Dockerfile"
            writeD "producer" "Dockerfile"
            c <- pwdConsumer
            p <- pwdProducer
            return (c, p, newDir)
            where
                writeD s ext = dockerfileWrite ("./" <> s <> "/" <> ext) . toDocker =<< addHarness s =<< testingPikaEnv s

        body (c, p, _) = do
            print =<< getCurrentDirectory
            let c' = T.pack c
                p' = T.pack p
            execStateT (runProcess () (scriptConnect c' p')) def

        fini (c, p, originalDir) = do
            --removeFile c
            --removeFile p
            setCurrentDirectory originalDir

pwdConsumer :: IO FilePath
pwdConsumer = canonicalizePath "./consumer"

pwdProducer :: IO FilePath
pwdProducer = canonicalizePath "./producer"

testingPikaEnv :: String -> IO ContainerEnv
testingPikaEnv script = do
    return $ testingEnv
           & (containerEnv_copyDirs      <>~ [(["--chown=atidot python/"], "/home/atidot/")])
           . (containerEnv_installations <>~ [("pip3 install", ["pika"])])
           . (containerEnv_command       .~  Just ["python3", script <> ".py"])

scriptConnect :: Text
              -> Text
              -> Platform ()
scriptConnect p' c' = do
    p <- container p' --producer
    c <- container c' --consumer
    p |--> c
    return ()

locateHarness :: Sh Text
locateHarness = do
    loc <- firstLine <$> Sh.run "which" ["platform-harness"]
    if (T.unpack loc) =~ ("^which: no" :: String)
       then throwM NoHarnessException
       else return loc

addHarness :: String
           -> ContainerEnv
           -> IO ContainerEnv
addHarness s cEnv = do
    pwd <- getCurrentDirectory
    createDirectoryIfMissing True $ pwd <> "/" <> s <> "/bin/"
    harnessLoc <- T.unpack <$> shelly locateHarness
    copyFile harnessLoc $ pwd <> "/" <> s <> "/bin/platform-harness"
    return $ cEnv
           & (containerEnv_copyDirs  <>~ [(["--chown=atidot bin/"], "/home/atidot/.local/bin/")])
           . (containerEnv_entrypoint %~ fmap harnessWrapEntrypoint)
           . (containerEnv_command    %~ fmap harnessWrapCommand)

harnessWrapEntrypoint :: String -> String
harnessWrapEntrypoint s = "platform-harness --entrypoint " <> entrypoint <> mconcat args
    where
        entrypoint = (head . words $ s) <> " "
        args = map (\(a, b) -> a <> b) $ zip keys vals
        keys = map (const "--entrypointArgs ") [1..]
        vals = map (<> " ") . tail' . words $ s
        tail' xs
            | xs == []  = []
            | otherwise = tail xs

harnessWrapCommand :: [String] -> [String]
harnessWrapCommand xs = words . harnessWrapEntrypoint . unwords $ xs
