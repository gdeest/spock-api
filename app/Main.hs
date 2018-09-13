{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config
import Control.Monad.Trans
import Data.IORef
import Data.Monoid
import qualified Data.Text as T

data Session = EmptySession
data AppState = AppState (IORef Int)

type Handler a = ActionT (WebStateM () Session AppState) a

main :: IO ()
main =
    do ref <- newIORef 0
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (AppState ref)
       runSpock 8080 (spock spockCfg app)

rootHandler :: Handler a
rootHandler = text "Hello World!"

greetHandler :: T.Text -> Handler a
greetHandler name = do
  AppState ref <- getState
  visitorNumber <- liftIO $
    atomicModifyIORef' ref $ \i -> (i+1, i+1)
  text ( "Hello " <> name
         <> ", you are visitor number "
         <> T.pack (show visitorNumber) )

app :: SpockM () Session AppState ()
app = do
  get root rootHandler
  get ("hello" <//> var) greetHandler
