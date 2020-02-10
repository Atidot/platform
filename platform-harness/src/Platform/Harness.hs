{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module Platform.Harness where

import "base" Data.Semigroup ((<>))
import "text" Data.Text (Text, append)
import "shelly" Shelly (Sh, run)

newtype MessengerProfile = MessengerProfile ()

-- The type of each Messenger is measured from the perspective of a particular
-- container. I.e. if A --> B --> C are one-way channels, then according to A, B is a Consumer
-- but according to C, B is a Producer.
data Messenger
    = Consumer MessengerProfile
    | Producer MessengerProfile

type Messengers = [Messenger]

setEnv :: Messengers -> Sh ()
setEnv messengers = do
    run "export" ["PLATFORM_HARNESS=1"]
    mapM_ setMessengerVar messengers

setMessengerVar :: Messenger -> Sh Text
setMessengerVar (Consumer cProfile) = run "export" . return $ varAppend "PLATFORM_CONSUMERS" (toBash cProfile)
setMessengerVar (Producer pProfile) = run "export" . return $ varAppend "PLATFORM_PRODUCERS" (toBash pProfile)

toBash :: MessengerProfile -> Text
toBash = undefined

varAppend :: Text -> Text -> Text
varAppend varName newVal = varName <> "=$" <> varName <> ":" <> newVal

getEnv :: IO Messengers
getEnv = undefined
