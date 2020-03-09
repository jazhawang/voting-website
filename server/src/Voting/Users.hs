
{-# LANGUAGE OverloadedStrings #-}

module Voting.Users (User, queryUsers, queryUser) where

import Voting.Types
import Database.PostgreSQL.Simple
import EnvHandler
import Servant
import Control.Monad.IO.Class

queryUsers :: Connection -> EnvHandler [User]
queryUsers conn = liftIO $ query_ conn "select * from User"
 
queryUser :: Connection -> Integer -> EnvHandler User
queryUser conn id = do    
    result <- liftIO (query conn "select * from User where id=?" [id])
    case result of 
        [user] -> return user
        _      -> throwError err404

