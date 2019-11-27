{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Platform.Packaging.PythonImports where

import "base" Control.Monad.IO.Class (MonadIO, liftIO)
import "base" Control.Monad (when, unless)
import "base" Data.Typeable (Typeable)
import "base" Data.Data (Data)
import "base" Data.List (foldl')
import "base" GHC.Generics (Generic)
import "base" System.IO
import "aeson" Data.Aeson (FromJSON, ToJSON, toEncoding, genericToEncoding, defaultOptions)
import "data-default" Data.Default (def)
import "text" Data.Text (Text, pack)
import "extra"      Data.Tuple.Extra ((&&&))
import "exceptions" Control.Monad.Catch (MonadMask, MonadThrow, bracket)
import "regex-tdfa" Text.Regex.TDFA
import "language-python" Language.Python.Common.AST 
import "language-python" Language.Python.Version2.Parser as V2
import "language-python" Language.Python.Version3.Parser as V3
import Platform.Packaging.Pip

type URL = Text

data PyPkg 
    = PyPkg
    { _pyPkg_index :: !URL
    , _pyPkg_name :: !Text
    } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance ToJSON PyPkg where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON PyPkg where

data ModuleName = ModuleName { _moduleName :: !Text }
    deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance ToJSON ModuleName where 
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ModuleName where

searchAndListNames :: (MonadMask m, MonadIO m)
                   => Text
                   -> m [Text]
searchAndListNames pkg = do
    t <- liftIO $ search def def pkg
    return . getAllTextMatches $ t =~ "^[^ ]+(?= )"

pypiPkg :: Text -> PyPkg
pypiPkg = PyPkg "https://pypi.org/simple/"

-- TODO: write some kind of exception instance for Language.Python.Common.ParseError
getAST :: (MonadThrow m, MonadMask m, MonadIO m) 
       => FilePath 
       -> m (Module annot)
getAST fp = do
    let fileName = fp =~ "(?<=/)[^/]+$" -- capture from the final slash to EOL
    when (fileName == "") $ mThrow EXCEPTION
    handle <- openFile fp ReadMode
    contents <- hGetContents handle
    let parsed = V3.parseModule contents fileName
    let finalParsed = if isRight parsed 
                         then parsed 
                         else V2.parseModule contents fileName
    unless (isRight finalParsed) $ mThrow EXCEPTION
    return finalParsed
    where isRight (Right _) = True
          isRight _         = False 

findPossibleMatches :: (MonadMask m, MonadIO m)
                    => ModuleName 
                    -> (ModuleName, m [PyPkg])
findPossibleMatches = id &&& (map pypiPkg <$> searchAndListNames def def . _moduleName)

findMatch :: (MonadMask m, MonadIO m, MonadThrow m)
          => ModuleName 
          -> [PyPkg] 
          -> m PyPkg
findMatch mn pkgs = do
    candidates <- filter (pkgHasModule mn) pkgs
    when (null candidates) $ mThrow EXCEPTION
    return $ head candidates

pkgHasModule :: PyPkg 
             -> ModuleName
             -> IO Bool
pkgHasModule = undefined

getImportNames :: Module annot -> [DottedName annot]
getImportNames (Module statements) = onlyJust . concatMap getImports' $ statements
  where getImports' :: Statement annot -> [Maybe (DottedName annot)]
        getImports' i@Import{}      = map (return . import_item_name) . import_items $ i
        getImports' f@FromImport{}  = return . import_relative_module . from_module $ f
        getImports' w@While{}       = recurse . (while_body <> while_else) $ w
        getImports' f@For  {}       = recurse . (for_body <> for_else) $ f
        getImports' a@AsyncFor{}    = recurse . return . for_stmt $ a
        getImports' f@Fun{}         = recurse . fun_body $ f
        getImports' a@AsyncFun{}    = recurse . return . fun_def $ a
        getImports' c@Class{}       = recurse . class_body $ c
        getImports' c@Conditional{} = recurse . mconcat . ((map snd . cond_guards) <> (return . cond_else)) $ c
        getImports' d@Decorated{}   = recurse . return . decorated_def $ d
        getImports' t@Try{}         = recurse . (try_body <> try_else <> try_finally) $ t
        getImports' w@With{}        = recurse . with_body $ w
        getImports' a@AsyncWith{}   = recurse . return . with_stmt $ a
        getImports' _               = []

        recurse :: [Statement annot] -> [Maybe (DottedName annot)]
        recurse = concatMap getImports'

        onlyJust :: [Maybe a] -> [a]
        onlyJust (Nothing : xs) = onlyJust xs
        onlyJust (Just x : xs)  = x : onlyJust xs
        onlyJust []             = []

-- for TopLevel.MidLevel.ModName, this would guess
--   "TopLevel" > "TopLevel MidLevel" > "TopLevel MidLevel ModName"
-- > "MidLevel" > "MidLevel ModName" > "ModName"
-- Perhaps there are better guessing orders, but this one seems pretty reasonable.
pkgGuesses :: DottedName annot -> [Text]
pkgGuesses = map (pack . unwords) . supLevelSets . map ident_string
    where supLevelSets (x:xs) = (x:xs) : supLevelSets xs
          supLevelSets [] = []

runPythonImports :: (MonadMask m, MonadIO m)
                 => FilePath
                 -> m [PyPkg]
runPythonImports fp
    = bracket init'
              fini
              body
    where
        init'  = return ()
        fini _ = return ()
        
        body _ = do
            importNames <- getImportNames <$> getAST fp
            possibleMatchesByImport <- map findPossibleMatches importNames
            let matchActions = curry findMatch possibleMatchesByImport
            return []
