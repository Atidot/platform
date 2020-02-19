{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}

module Platform.Harness.Types
    ( HarnessCmd(..)
    , HarnessScript(..)
    , HarnessT
    ) where

import "base" Data.Semigroup (Semigroup, (<>))
import "base" Data.Data (Data)
import "base" Data.Typeable (Typeable)
import "base" GHC.Generics (Generic)
import "aeson" Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import "mtl" Control.Monad.Writer.Lazy (WriterT)
import "platform-types" Platform.Types

data HarnessCmd
    = Queue QueueID
    | Produce ContainerID QueueID
    | Consume ContainerID QueueID
    | Failure
    deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance ToJSON HarnessCmd where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON HarnessCmd where

-- The HarnessScript needs to summarize a monadic script
-- already run in a more complex interpreter.
--
-- The HarnessScript needs to be a simple monoid for two reasons:
-- 1) we limit the complexity of malicious code execution available if the script-passing
--    channel is compromised.
-- 2) we have an easy time encoding it into JSON, whereas anything
--    with a functor instance becomes very hard to encode in general.
newtype HarnessScript = HarnessScript { commands :: [HarnessCmd] }
    deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance Semigroup HarnessScript where
    (<>) (HarnessScript s1) (HarnessScript s2) = HarnessScript (s1 <> s2)

instance Monoid HarnessScript where
    mempty = HarnessScript mempty

instance ToJSON HarnessScript where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON HarnessScript where

type HarnessT m a = WriterT HarnessScript m a
