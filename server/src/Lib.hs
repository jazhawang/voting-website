{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Lib (app) where
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Servant
import Data.Aeson
import Data.Aeson.Types
import Data.Time.Calendar
import GHC.Generics

type RootEndpoint = Get '[JSON] String

type APIEndpoints = "api" :> "v1" :> (
  "users" :> Get '[JSON] [User] 
  :<|> "message" :> Get '[JSON] String 
  )

type API = RootEndpoint :<|> APIEndpoints


appServer :: Server API
appServer = (logServer "Accesing Root Endpoint" >> return "Hello, World!")
       :<|> return users
       :<|> (logServer "🚀  Launch the missles!!!" >> return "Missiles emminent 🚀🚀")

appAPI :: Proxy API
appAPI = Proxy

app :: Application
app = serve appAPI appServer


-- simple logging helper function
logServer :: String -> Handler ()
logServer = liftIO . putStrLn 

-- User datatype and instances, remove later --
data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)
instance ToJSON User

users :: [User]
users =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
  ]
