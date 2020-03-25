{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Platform.Packaging.PythonImports.Internal.Download (interruptPipDownload) where

import           "base"         Debug.Trace (trace)
import           "base"         System.IO (Handle, hGetLine)
import           "base"         Control.Monad (forM)
import           "base"         Data.Maybe (maybe)
import           "base"         Data.Semigroup (Last(..), getLast, (<>))
import           "exceptions"   Control.Monad.Catch (Exception, throwM)
import           "monad-loops"  Control.Monad.Loops (iterateWhile)
import           "transformers" Control.Monad.Trans.Writer (WriterT, tell, execWriterT)
import           "transformers" Control.Monad.Trans.Class (lift)
import           "regex-pcre"   Text.Regex.PCRE
import           "text"         Data.Text (Text, pack)
import qualified "shelly"       Shelly as Sh (FilePath)
import           "shelly"       Shelly hiding (trace)

-- Believe it or not, `pip download` does much more than just download a Python package.
-- It actually executes the first steps of a build.
-- For instance, `pip download pandas` will invoke a compiler! This was causing `platform-packaging`
-- to throw exceptions.
-- In order to use `pip download` for static analysis, we need to do the following:
-- 1. Invoke `pip download` with a verbose output
-- 2. Watch this output for of `pip download` in order to interrupt it once the correct URL of a package is found.
-- 3. Use `wget` to perform a simple download.

type URL = String

data LoopState
    = StillLooping
    | Finished
    deriving (Show, Eq)

data DownloadException
    = NoLinksFoundException
    deriving (Show, Eq)

instance Exception DownloadException

interpretDownloadStream :: [Text] -> Sh URL
interpretDownloadStream opts
  = trace "INTERPRETING STREAM"
  $ runHandle (fromText "pip3") ("download" : opts) handleStream

-- The following infinite loop terminates with a NoLinksFoundException
-- or when 'exit' is explicitly called in 'recordCorrectURL'.
handleStream :: Handle -> Sh URL
handleStream h = do
    let iteration = recordCorrectURL h
        loop      = iterateWhile (== StillLooping) iteration
    url' <- execWriterT loop
    return $ maybe
             (trace "Terminating via error" $ throwM NoLinksFoundException)
             getLast
             url'

recordCorrectURL :: Handle -> WriterT (Maybe (Last URL)) Sh LoopState
recordCorrectURL handle = do
    l <- liftIO $ hGetLine handle
    trace ("Got line : " <> l) $ return ()
    let linkFound = l =~ foundLinkRegex :: Bool
    tell . Just . Last $ (l =~ foundLinkRegex :: URL)
    if linkFound
       then trace ("Found actual link in string: " <> l) $ return Finished
       else return StillLooping

interruptPipDownload :: [Text] -> Sh ()
interruptPipDownload opts = do
    url <- interpretDownloadStream opts
    trace "RUNNING WGET" $ run_ (fromText "wget") ["-q", pack url]

-- match example:    Found link https://files.pythonhosted.org/packages/2f/79/f236ab1cfde94bac03d7b58f3f2ab0b1cc71d6a8bda3b25ce370a9fe4ab1/pandas-1.0.3.tar.gz#sha256=32f42e322fb903d0e189a4c10b75ba70d90958cc4f66a1781ed027f1a1d14586 (from https://pypi.org/simple/pandas/) (requires-python:>=3.6.1), version: 1.0.3
foundLinkRegex :: String
foundLinkRegex = "(?<=Downloading from URL )https?:\\/\\/[^\\s#]+(\\.whl|\\.tar.gz)(?=(\\s|#))"
