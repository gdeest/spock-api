{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Web.Spock
import Web.Spock.Config
import Control.Monad.Trans
import Data.Aeson hiding (json)
import Data.IORef
import Data.Monoid
import GHC.Generics
import Network.Wai.Middleware.RequestLogger

import qualified Data.Text as T

data Session = EmptySession
data AppState = AppState (IORef Int)

type Handler a = ActionT (WebStateM () Session AppState) a

data User = User
  { firstName :: T.Text
  , lastName :: T.Text
  }
  deriving (Generic)

instance ToJSON User

main :: IO ()
main =
    do ref <- newIORef 0
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (AppState ref)
       runSpock 8080 $ fmap (logStdoutDev .) $ (spock spockCfg app)

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
  get "json" $ json $ User "Gaël" "Deest"
