{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Platform.Packaging.PythonImports.Internal.Download (interruptPipDownload) where

import           "base"         Debug.Trace (trace)
import           "base"         System.IO (Handle, hGetLine, hClose)
import           "base"         Control.Monad (forM)
import           "base"         Data.IORef
import           "base"         Data.List ((\\))
import           "base"         Data.Maybe (maybe)
import           "base"         Data.Semigroup ((<>))
import           "exceptions"   Control.Monad.Catch (handleAll, Exception, throwM)
import           "monad-loops"  Control.Monad.Loops (iterateWhile)
import           "transformers" Control.Monad.Trans.Writer (WriterT, tell, execWriterT)
import           "transformers" Control.Monad.Trans.Class (lift)
import           "regex-pcre"   Text.Regex.PCRE
import           "text"         Data.Text (Text, pack)
import qualified "text"         Data.Text as T (lines)
import qualified "shelly"       Shelly as Sh (FilePath)
import           "shelly"       Shelly hiding (trace)

-- TODO: Rewrite handleInterrupt to be more selective about which errors are silenced.

-- Believe it or not, `pip download` does much more than just download a Python package.
-- It actually executes the first steps of a build.
-- For instance, `pip download pandas` will invoke a compiler! This was causing `platform-packaging`
-- to throw exceptions. And allowing pip to run code off the internet is less secure than a static
-- analysis of the package files.
-- In order to use `pip download` for static analysis, we need to do the following:
-- 1. Invoke `pip download` with a verbose output
-- 2. Watch this output for of `pip download` until pip decides on a URL to download from.
-- 3. Interrupt pip.
-- 4. Use `wget` to download that file instead.

type URL = String

data LoopState
    = StillLooping
    | Finished
    deriving (Show, Eq)

data DownloadException
    = RaceCondition
    deriving (Show, Eq)

instance Exception DownloadException

-- | Use pgrep to find pip (as well as anything with "pip" in its name, like "pipe").
-- The caller should check for race conditions etc.
pipGrep :: Sh [Text]
pipGrep = filter (/= "") . T.lines <$> run (fromText "pgrep") ["pip"]

-- | We use an IORef because the pip process will die via the TERM signal. The resulting
-- exception will interrupt the normal control flow, and "output by pointer" is the simplest
-- workaround here.
interpretDownloadStream :: IORef URL -- ^ We will write the output here
                        -> [Text]    -- ^ Arguments for pip download
                        -> Sh ()
interpretDownloadStream output opts = do
    originalPipJobs <- pipGrep
    runHandle (fromText "pip3") ("download" : opts) (handleStream output originalPipJobs)

handleStream :: IORef URL -- ^ We will write the output here
             -> [Text]    -- ^ Original pip jobs we don't want to kill.
             -> Handle    -- ^ Handle of the stdout of the new pip job.
             -> Sh ()     -- ^ The URL which pip decided to download from.
handleStream ref originalPips h = do
    let iteration = recordCorrectURL h
        loop      = iterateWhile (== StillLooping) iteration
    currentPips  <- pipGrep
    url          <- execWriterT loop
    let newPip    = currentPips \\ originalPips
    when (length newPip /= 1)
         (throwM RaceCondition)
    liftIO $ hClose h
    liftIO $ writeIORef ref url
    -- Terminate 'newPip' if it has not already terminated.
    -- This prevents IO exceptions related to build processes
    -- `pip download` causes with some packages.
    run_ (fromText "kill") newPip

recordCorrectURL :: Handle -> WriterT URL Sh LoopState
recordCorrectURL handle = do
    l <- liftIO $ hGetLine handle
    let linkFound = l =~ foundLinkRegex :: Bool
    tell (l =~ foundLinkRegex :: URL)
    if linkFound
       then return Finished
       else return StillLooping

interruptPipDownload :: [Text] -> Sh ()
interruptPipDownload opts = do
    urlHolder <- liftIO $ newIORef ""
    handleInterrupt $ interpretDownloadStream urlHolder opts
    url <- liftIO $ readIORef urlHolder
    trace "RUNNING WGET" (run (fromText "wget") [pack url])
    return ()
    where
        handleInterrupt = handleAll
                        $ \e -> trace ("An exception was thrown in interpretDownloadStream: " <> show e) (return ())

-- match example:    Found link https://files.pythonhosted.org/packages/2f/79/f236ab1cfde94bac03d7b58f3f2ab0b1cc71d6a8bda3b25ce370a9fe4ab1/pandas-1.0.3.tar.gz#sha256=32f42e322fb903d0e189a4c10b75ba70d90958cc4f66a1781ed027f1a1d14586 (from https://pypi.org/simple/pandas/) (requires-python:>=3.6.1), version: 1.0.3
foundLinkRegex :: String
foundLinkRegex = "(?<=Downloading from URL )https?:\\/\\/[^\\s#]+(\\.whl|\\.tar\\.gz)(?=(\\s|#))"
