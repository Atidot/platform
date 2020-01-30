{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Platform.Packaging.PythonImports.Internal.Annotation where

import           "base"                     Control.Monad (filterM)
import           "base"                     Data.Maybe (listToMaybe)
import           "text"                     Data.Text (Text, pack)
import           "regex-pcre"               Text.Regex.PCRE
import           "language-python"          Language.Python.Common.AST
import           "language-python"          Language.Python.Common.Token (Token, token_literal, token_span)
import           "language-python"          Language.Python.Common.SrcLocation (SrcSpan(SpanCoLinear))
import           "language-python"          Language.Python.Version3.Parser (parseStmt)
import           "platform-packaging-types" Platform.Packaging.Pip.Types
import           "platform-packaging-types" Platform.Packaging.PythonImports.Types
import                                      Platform.Packaging.PythonImports.Internal.Utils

getExplicitPkgOrigins :: String
                      -> Maybe [(ModuleName, PyPkg)]
getExplicitPkgOrigins fileContents = do
    comments <- getComments fileContents
    let annotationFiltered = filter hasPkgAnnotation comments
    explicitPkgs <- filterM (comesAfterImport fileContents) annotationFiltered
    fmap (map (\(dn, pkg) -> (dottedToModuleName dn, pkg))) $ mapM (processPkg fileContents) explicitPkgs

processPkg :: String
           -> Token
           -> Maybe (DottedName SrcSpan, PyPkg)
processPkg contents tkn = do
    importedMod <- fmap ( getForeignImportNames
                 . Module
                 . return)
                 $ stmtBeforeComment contents tkn
    fmap (\dn -> (dn, pkgFromToken tkn))
         $ listToMaybe importedMod

comesAfterImport :: String
                 -> Token
                 -> Maybe Bool
comesAfterImport contents token = return . isImport $ stmtBeforeComment contents token

hasPkgAnnotation :: Token -> Bool
hasPkgAnnotation tkn
  = (token_literal tkn) =~ explicitPkgRegex
 || (token_literal tkn) =~ suppressPkgRegex

stmtBeforeComment :: String
                  -> Token
                  -> Maybe (Statement SrcSpan)
stmtBeforeComment contents tkn = do
    file <- fileFromCommentLine contents tkn
    case parseStmt file "" of
      Right (stmt : stmts, _) -> Just stmt
      _                       -> Nothing

fileFromCommentLine :: String
                    -> Token
                    -> Maybe String
fileFromCommentLine fileContents tkn = do
    beginning <- fmap (+(-1)) . commentLine . token_span $ tkn
    return . unlines . drop beginning . lines $ fileContents

isImport :: Maybe (Statement annot) -> Bool
isImport (Just Import{})     = True
isImport (Just FromImport{}) = True
isImport _                   = False

commentLine :: SrcSpan -> Maybe Int
commentLine (SpanCoLinear _ row _ _) = return row
commentLine _                        = Nothing

explicitPkgRegex :: String
explicitPkgRegex = "(?<=!platform )(\\w|\\d)(\\w|\\d|-)*(?=( |\\t)*$)"

-- This matches on a `!platform`-prepended comment but does not capture anything.
suppressPkgRegex :: String
suppressPkgRegex = "(?<=!platform)(?=( |\\t)*$)"

pkgFromToken :: Token -> PyPkg
pkgFromToken tkn = pypiPkg . pack $ (token_literal tkn =~ explicitPkgRegex)
