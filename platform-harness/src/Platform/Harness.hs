{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module Platform.Process where

import "data-text" Data.Text (Text, append, (<>))
import "shelly" Shelly

newtype ConsumerProfile = ConsumerProfile ()
newtype ProducerProfile = ProducerProfile ()

data Messenger = Consumer ConsumerProfile | Producer ProducerProfile

type Messengers = [Messenger]

setEnv :: Messengers -> Shelly ()
setEnv = run_ "export PLATFORM_HARNESS=1"

setMessengerVar :: Messenger -> Shelly ()
setMessengerVar Consumer cProfile = run_ $ varAppend "PLATFORM_CONSUMERS" (toBash cProfile)
setMessengerVar Producer pProfile = run_ $ varAppend "PLATFORM_PRODUCERS" (toBash pProfile)

toBash :: ConsumerProfile -> Text
toBash = undefined
toBash :: ProducerProfile -> Text
toBash = undefined

varAppend :: Text -> Text -> Text
varAppend varName newVal = "export " <> varName <> "=$" <> varName <> ":" <> newVal

getEnv :: IO Messengers
getEnv = undefined
