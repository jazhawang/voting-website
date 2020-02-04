{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Users (User, queryUsers, queryUser) where
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Time.Calendar
import GHC.Generics
import Database.PostgreSQL.Simple
import EnvHandler
import Servant
import Servant.Server
import Control.Monad.IO.Class
import AppEnv

data User = User
  { userID :: Integer
  , username :: String  
  , dateJoined :: Day
  , email :: String
  } deriving (Eq, Show, Generic, FromRow, ToRow)
instance ToJSON User
instance FromJSON User

queryUsers :: Connection -> EnvHandler [User]
queryUsers conn = liftIO $ query_ conn "select * from users"
 
queryUser :: Connection -> Integer -> EnvHandler User
queryUser conn id = do    
    result <- liftIO (query conn "select * from users where userid=?" [id])
    case result of 
        [user] -> return user
        _      -> throwError err404
