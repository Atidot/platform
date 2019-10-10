{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Platform.Visual where

import "base"           Control.Monad.IO.Class (MonadIO, liftIO)
import "base"           Data.Maybe (fromJust)
import "base"           Data.List (elemIndex, nub, sort)
import "lens"           Control.Lens
import "text"           Data.Text (Text)
import "data-default"   Data.Default (Default, def)
import "mtl"            Control.Monad.State.Class (MonadState, gets)
import "exceptions"     Control.Monad.Catch (MonadMask, bracket)
import "free"           Control.Monad.Free
import "fgl"            Data.Graph.Inductive.Graph
import "fgl"            Data.Graph.Inductive.PatriciaTree
import "graphviz"       Data.GraphViz
import "platform-types" Platform.Types
import "platform-dsl"   Platform.DSL

---------
-- TODO: move to another file
type VisualConfig = ()


data VisualState
    = VisualState
    { _visualState_nodes   :: !([Text])
    , _visualState_edges   :: !([(Text, Text, Text)])
    , _visualState_graph   :: !(Gr Text Text)
    , _visualState_dot     :: !(DotGraph Int)
    } deriving (Show, Read, Eq)
makeLenses ''VisualState

instance Default VisualState where
    def = VisualState []
                      []
                      empty
                      (graphToDot nonClusteredParams (empty :: Gr Text Text))

---------
runVisual :: (Monad m, MonadState VisualState m, MonadMask m)
          => VisualConfig
          -> Platform a
          -> m a
runVisual config script
    = bracket init'
              fini
              body
    where
        init'  = return ()
        fini _ = return ()
        body _ = do
            result <- iterM run script

            -- build an FGL graph
            ns <- gets _visualState_nodes
            es <- gets _visualState_edges
            let graph = toFGL ns es
            visualState_graph .= graph

            -- draw with graphviz
            let dot = graphToDot nonClusteredParams graph
            visualState_dot .= dot

            return result


        run :: (Monad m, MonadState VisualState m, MonadMask m)
             => PlatformCmd (m a)
             -> m a
        run (Container name return') = do
            -- add a node to the graph
            visualState_nodes <>= [name]

            -- return new ContainerID
            let newContainer = ContainerID name
            return' newContainer


        run (Connection (ContainerID name1)
                        (ContainerID name2)
                        return'
            ) = do
            -- add an edge to the graph
            visualState_edges <>= [(name1, name2, "TODO")]
            return'


toFGL :: [Text] -> [(Text, Text, Text)] -> Gr Text Text
toFGL ns es = graph
    where
        uniqNs :: [Text]
        uniqNs = nub . sort $ ns

        label :: Text -> Int
        label  = fromJust . flip elemIndex uniqNs

        lNodes :: [(Int, Text)]
        lNodes = map (\n -> (label n, n)) ns

        lEdges :: [(Int, Int, Text)]
        lEdges = map (\(n1, n2, t) -> (label n1, label n2, t)) es

        graph :: Gr Text Text
        graph  = mkGraph lNodes lEdges


