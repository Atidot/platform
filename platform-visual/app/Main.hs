{-# LANGUAGE PackageImports #-}
module Main where

import           "base"                    Data.Monoid (mempty)
import           "base"                    System.IO (stdin)
import qualified "text"                    Data.Text.Lazy as T (unpack)
import           "data-default"   Data.Default (Default, def)
import           "optparse-applicative"    Options.Applicative
import           "mtl"                     Control.Monad.State (execStateT, evalStateT)
import           "graphviz"                Data.GraphViz
import           "graphviz"                Data.GraphViz.Printing
import           "platform-types"          Platform.Types
import           "platform-dsl"            Platform.DSL
import qualified "platform-dsl"            Platform.DSL as DSL (test)
import           "platform-visual"         Platform.Visual


main :: IO ()
main = do
    newState <- flip execStateT def $ runVisual () DSL.test
    let dot = _visualState_dot newState
    putStrLn . T.unpack . renderDot . toDot $ dot
    return ()
