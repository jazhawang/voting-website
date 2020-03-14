{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Lib (app, AppEnv(..), EnvHandler) where
import Control.Monad.Except
import Control.Monad.Reader
import Servant
import Voting.Member
import Voting.Topic
import Voting.Types
import Data.Pool
import EnvHandler
import AppEnv

type RootEndpoint = Get '[JSON] String
type APIEndpoints = "api" :> "v1" :> (APIEndpointsMembers :<|> APIEndpointsTopics)
type API = RootEndpoint :<|> APIEndpoints

appServer :: ServerT API EnvHandler
appServer = rootServer :<|> apiServer

rootServer :: EnvHandler String
rootServer = logServer "Accesing Root Endpoint" >> return "Hello, World!"

apiServer = apiMemberServer :<|> apiTopicServer

appAPI :: Proxy API
appAPI = Proxy

app :: AppEnv -> Application
app env = serve appAPI $ hoistServer appAPI (envHandlerToHandler env) appServer

-- simple logging helper function
logServer :: String -> EnvHandler ()
logServer = liftIO . putStrLn 

type APIEndpointsMembers = 
       ("members"                                       :> Get '[JSON] [Member])
  :<|> ("member" :> Capture "id" Integer                :> Get '[JSON] Member)
  :<|> ("member" :> Capture "id" Integer :> "votes"     :> Get '[JSON] [Vote])
  :<|> ("member" :> Capture "id" Integer :> "allocated" :> Get '[JSON] [AllocatedVote])
  :<|> ("member" :> Capture "id" Integer :> "topic" :> Capture "id" Integer 
          :> Get '[JSON] MemberOnTopic)

apiMemberServer = 
       (asks db >>= \x -> withResource x queryMembers)
  :<|> (\id -> asks db >>= \x -> withResource x (\con -> queryMember con id))
  :<|> (\id -> asks db >>= \x -> withResource x (\con -> queryMemberVotes con id))  
  :<|> (\id -> asks db >>= \x -> withResource x (\con -> queryMemberAllocatedVotes con id)) 
  :<|> (\mid tid -> asks db >>= \x -> withResource x (\con -> queryMemberTopic con mid tid)) 


type APIEndpointsTopics =
  ("topics" :> Get '[JSON] [Topic])
  :<|> ("topic" :> Capture "id" Integer :> Get '[JSON] FullTopic)
  :<|> ("topic" :> Capture "id" Integer :> "members" :> Get '[JSON] [MemberAllocated])

apiTopicServer = 
  (asks db >>= \x -> withResource x queryTopics)  
  :<|> (\id -> asks db >>= \x -> withResource x (\con -> queryTopicFull con id))
  :<|> (\id -> asks db >>= \x -> withResource x (\con -> queryTopicMembers con id))
  
