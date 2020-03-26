{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE OverloadedStrings #-}
module Platform.Packaging.PythonImports.Internal.PipMatches.ParseStdLibHTML (scrapeModules) where

import "base"    Data.String (fromString)
import "scalpel" Text.HTML.Scalpel

scrapeModules :: IO (Maybe [String])
scrapeModules = scrapeURL "https://docs.python.org/3/py-modindex.html" modules
    where
        modules :: Scraper String [String]
        modules = chroot ("table" @: [hasClass "modindextable"])
                $ moduleLinks --text (fromString "code")

        moduleLinks :: Scraper String [String]
        moduleLinks = chroots ("a" @: []) xrefCode

        xrefCode :: Scraper String String
        xrefCode = text $ "code" @: [hasClass "xref"]