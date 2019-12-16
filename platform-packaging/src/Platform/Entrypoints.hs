{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Platform.Packaging.Entrypoints where

import "text" Data.Text
import "language-python" Language.Python.Common.AST
import "language-python" Language.Python.Common.SrcLocation
import "language-python" Language.Python.Version3.Parser (parseModule)

-- findEntrypoint should find a SpanMultiLine or a SpanCoLinear
findEntrypoints :: String -> SrcSpan
findEntrypoint = undefined

isEntrypoint :: String -> SrcSpan -> Bool
isEntrypoint contents span 
  = case parseModule (subsection contents span) of
      Left _ = False
      Right parsed = if not $ isDecoratedFunction parsed
                        then False
                        else takesDataframe parsed
    where
        isDecoratedFunction [Decorated _ Fun{} _] = True
        isDecoratedFunction _                     = False
        takesDataFrame = any . map (`in` dfArgNames) . decorated_def . head
        dfArgNames = ["df"]

-- subsection will return Nothing if it is asked to return a subsection of its input that doesn't exist.
subsection :: String -> SrcSpan -> Maybe String
subsection s (SpanCoLinear _ row startCol endCol) 
  = take' endCol . drop' (startCol - 1) . unlines . take' 1 . drop' (row - 1) . fmap lines $ Just s
subsection s (SpanMultiLine _ startRow endRow startCol endCol)
  = drop' (startCol - 1) . fmap unlines . delLast endCol . take' endRow . drop' (startRow - 1) . fmap lines $ Just s
      where
          delLast n = fmap (delLast' n)
          delLast' n []       = []
          delLast' n (x : []) = take n x : []
          delLast' n (x : xs) = x : delLast n xs
subsection s (SpanPoint _ row col)
  = take 1 . drop (col - 1) . unlines . take row . drop (row - 1) . lines $ s
subsection s SpanEmpty
  = []

-- lengthcheck helps us generate the helper functions take' and drop' for use in subsection.
lengthCheck :: (Int -> [a] -> [a]) -> Int -> Maybe [a] -> Maybe [a]
lengthCheck f n as = join . fmap $ \as -> if n < 0 || n > length as
                                     then Nothing
                                     else Just (f n as)

take' :: Int -> Maybe [a] -> Maybe [a]
take' = lengthCheck take

drop' :: Int -> Maybe [a] -> Maybe [a]
drop' = lengthCheck drop

generateMain :: String -> Maybe String
generateMain = undefined
