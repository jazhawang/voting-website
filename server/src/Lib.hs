{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Lib (app, AppEnv(..), EnvHandler) where
import Control.Monad.Except
import Control.Monad.Reader
import Servant
import Voting.Member
import Voting.Types
import Data.Pool
import EnvHandler
import AppEnv


type RootEndpoint = Get '[JSON] String

type APIEndpoints = "api" :> "v1" :> APIEndpointsMembers

type APIEndpointsMembers = 
       ("members" :> Get '[JSON] [Member]) -- get all Members
  :<|> ("member" :> Capture "id" Integer :> Get '[JSON] Member)

type API = RootEndpoint :<|> APIEndpoints

apiMemberServer = 
  (do cp <- asks db
      withResource cp queryMembers)
  :<|> (\id -> do 
    cp <- asks db
    withResource cp (\con -> queryMember con id))
    
appServer :: ServerT API EnvHandler
appServer = rootServer :<|> apiServer

rootServer :: EnvHandler String
rootServer = logServer "Accesing Root Endpoint" >> return "Hello, World!"

apiServer = apiMemberServer

appAPI :: Proxy API
appAPI = Proxy

app :: AppEnv -> Application
app env = serve appAPI $ hoistServer appAPI (envHandlerToHandler env) appServer

-- simple logging helper function
logServer :: String -> EnvHandler ()
logServer = liftIO . putStrLn 