
{-# LANGUAGE OverloadedStrings #-}

module Voting.Member (queryMember, queryMembers) where

import Voting.Types
import Database.PostgreSQL.Simple
import EnvHandler
import Servant
import Control.Monad.IO.Class

queryMembers :: Connection -> EnvHandler [Member]
queryMembers conn = liftIO $ query_ conn "select * from Member"
 
queryMember :: Connection -> Integer -> EnvHandler Member
queryMember conn id = do    
    result <- liftIO (query conn "select * from Member where id=?" [id])
    case result of 
        [member] -> return member
        _      -> throwError err404
