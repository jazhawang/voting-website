{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Lib (app, AppEnv(..), EnvHandler) where
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Servant
import Servant.Server
import Data.Aeson
import Data.Aeson.Types
import Data.Time.Calendar
import GHC.Generics
import Voting.Users
import Voting.Types
import Database.PostgreSQL.Simple
import Data.Pool
import EnvHandler
import AppEnv

type RootEndpoint = Get '[JSON] String

type APIEndpoints = "api" :> "v1" :> APIEndpointsUsers

type APIEndpointsUsers = 
       ("users" :> Get '[JSON] [User]) -- get all users
  :<|> ("user" :> Capture "id" Integer :> Get '[JSON] User)

type API = RootEndpoint :<|> APIEndpoints

apiUserServer = 
  (do cp <- asks db
      withResource cp queryUsers)
  :<|> (\id -> do 
    cp <- asks db
    withResource cp (\con -> queryUser con id))
    
appServer :: ServerT API EnvHandler
appServer = rootServer :<|> apiServer

rootServer :: EnvHandler String
rootServer = logServer "Accesing Root Endpoint" >> return "Hello, World!"

apiServer = apiUserServer

appAPI :: Proxy API
appAPI = Proxy

app :: AppEnv -> Application
app env = serve appAPI $ hoistServer appAPI (envHandlerToHandler env) appServer

-- simple logging helper function
logServer :: String -> EnvHandler ()
logServer = liftIO . putStrLn 