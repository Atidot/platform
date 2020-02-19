{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Platform.Harness.DSL (withHarnessDSLWriter) where

import "mtl" Control.Monad.Writer.Lazy (tell)
import "free" Control.Monad.Free (Free(..), foldFree)
import "transformers" Control.Monad.Trans.Class (lift)
import qualified "platform-dsl" Platform.DSL as DSL
import "platform-dsl" Platform.DSL (Platform, PlatformCmd, produce, consume, queue, container)
import Platform.Harness.Types

withHarnessDSLWriter :: Monad m => (PlatformCmd (m a) -> m a) -> PlatformCmd (m a) -> HarnessT m a
withHarnessDSLWriter interpreter cmd = undefined -- platformDSLToHarnessDSL cmd (interpreter cmd)

--platformDSLToHarnessDSL :: Monad m => PlatformCmd (m a) -> m a -> HarnessT m a
--platformDSLToHarnessDSL (DSL.Queue     _ _) val = lift val
--platformDSLToHarnessDSL (DSL.Container _ _) val = lift val
--platformDSLToHarnessDSL (DSL.Produce containerID queueID _) val = do
--    tell . HarnessScript . return $ Produce containerID queueID
--    lift val
--platformDSLToHarnessDSL (DSL.Consume containerID queueID _) val = do
--    tell . HarnessScript . return $ Consume containerID queueID
--    lift val
