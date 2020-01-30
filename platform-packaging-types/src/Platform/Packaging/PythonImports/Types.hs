{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Platform.Packaging.PythonImports.Types where

import           "base"                     Data.Typeable (Typeable)
import           "base"                     Data.Data (Data)
import           "base"                     GHC.Generics (Generic)
import           "aeson"                    Data.Aeson (FromJSON, ToJSON, toEncoding, genericToEncoding, defaultOptions)
import           "text"                     Data.Text (Text)
import           "exceptions"               Control.Monad.Catch (Exception)

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

data PythonImportException
    = ModuleInNoPackages
    | FileNotParseable
    | ImpossibleFileState
    | NoInitFile
    | NoTopLevelFile
    | NoSuchFile
    | MultipleSuchFiles
    deriving (Read, Eq, Ord, Bounded, Enum, Data, Typeable, Generic)

instance Show PythonImportException where
    show ModuleInNoPackages = "No package was found exporting the appropriate module name."
    show FileNotParseable = "This file could not be parsed as Python 3 code."

instance Exception PythonImportException

data PackageFormat
    = TarPackage
    | WheelPackage
    deriving (Read, Eq, Ord, Bounded, Enum, Data, Typeable, Generic)

data PkgVerdict
    = PkgContainsModule
    | PkgLacksModule
    | Inconclusive
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable, Data, Generic)
