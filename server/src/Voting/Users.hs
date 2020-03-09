{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Voting.Users (User, queryUsers, queryUser) where
import Control.Monad
import Voting.Types
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

queryUsers :: Connection -> EnvHandler [User]
queryUsers conn = liftIO $ query_ conn "select * from User"
 
queryUser :: Connection -> Integer -> EnvHandler User
queryUser conn id = do    
    result <- liftIO (query conn "select * from User where id=?" [id])
    case result of 
        [user] -> return user
        _      -> throwError err404

