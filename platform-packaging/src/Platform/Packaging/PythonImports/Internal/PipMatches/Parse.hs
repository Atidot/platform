{-# LANGUAGE PackageImports #-}
module Platform.Packaging.PythonImports.Internal.PipMatches.Parse (setupCfg) where

import "base" Data.Maybe (listToMaybe)
import "containers" Data.Map.Strict (Map)
import qualified "containers" Data.Map as M
import "parsec" Text.ParserCombinators.Parsec hiding (option)
import "language-python" Language.Python.Common.AST

type SetupCommands = Map String CommandOpts
type CommandOpts   = Map String String

type FunctionKwargs = Map String String

topLevelSetupKwargs :: Module annot -> Maybe CommandOpts
topLevelSetupKwargs (Module stmts) = listToMaybe
                                   . map toKwargs
                                   . filter isSetupStmt
                                   $ stmts

isSetupStmt :: Statement annot -> Bool
isSetupStmt (StmtExpr (Call (Var (Ident name _) _) _ _) _) = name == "setup"
isSetupStmt _                                              = False

toKwargs :: Statement annot -> Map String String
toKwargs = M.fromList
         . map (\(x,y) -> (ident_string x, concat . strings_strings $ y))
         . filter (isString . snd)
         . map (\a -> (arg_keyword a, arg_expr a))
         . filter isKwarg
         . call_args
         . stmt_expr

isString :: Expr annot -> Bool
isString Strings{} = True
isString _         = False

isKwarg :: Argument annot -> Bool
isKwarg ArgKeyword{} = True
isKwarg _            = False

setupCfg :: GenParser Char st SetupCommands
setupCfg = do
    many blankLine
    cmds <- many command
    many blankLine
    eof
    return $ M.fromList cmds

command :: GenParser Char st (String, CommandOpts)
command = do
    many blankLine
    char '['
    cmd <- many1 (noneOf specialChars)
    char ']'
    eol
    options <- many1 option
    return $ (cmd, M.fromList options)

option :: GenParser Char st (String, String)
option = do
    many blankLine
    key <- optionKey
    val <- optionVal (length key)
    eol
    return (key, val)

optionKey :: GenParser Char st String
optionKey = many1 (noneOf specialChars)

optionVal :: Int -> GenParser Char st String
optionVal len = do
    line1 <- manyTill anyChar (try eol)
    continuations <- many (optionValLine len)
    return (line1 ++ concat continuations)

optionValLine :: Int -> GenParser Char st String
optionValLine len = do
    many blankLine
    count len $ char ' '
    l <- manyTill anyChar (try eol)
    return ("\n" ++ l)

blankLine :: GenParser Char st String
blankLine = many (oneOf "\t ") >> eol

eol :: GenParser Char st String
eol = between (char '#') (string "\n" <|> string "\r\n") (many $ noneOf "\r\n")

specialChars :: String
specialChars = "#\r\n[]"
