{-# LANGUAGE OverloadedStrings #-}

module Voting.Member 
    ( queryMember
    , queryMembers
    , queryMemberVotes
    , queryMemberAllocatedVotes
    , queryMemberTopic
    ) where

import Voting.Types
import Voting.Util
import Database.PostgreSQL.Simple
import EnvHandler
import Servant
import Data.Time
import Data.Time.Clock
import Control.Monad.IO.Class
import Control.Lens hiding (Choice)

queryMembers :: Connection -> EnvHandler [Member]
queryMembers = get "SELECT * FROM Member"
 
queryMember :: Connection -> Integer -> EnvHandler Member
queryMember = getSingleByID "SELECT * FROM Member WHERE id=?"

queryMemberVotes :: Connection -> Integer -> EnvHandler [Vote]
queryMemberVotes = getByID "SELECT * FROM Vote WHERE voterID=?" 

queryMemberAllocatedVotes :: Connection -> Integer -> EnvHandler [AllocatedVote]
queryMemberAllocatedVotes = 
    getByID "SELECT * FROM AllocatedVote WHERE memberID=?" 

-- find the allocated votes and previous votes of a member for a given topic
-- will throw a 404 if the user has not been allocated any votes previously
queryMemberTopic :: Connection -> 
    Integer -> Integer -> EnvHandler MemberOnTopic
queryMemberTopic conn memberID topicID = do
    allocatedResult <- getByParams allocStr conn [memberID, topicID]
    allocatedSingle <- single allocatedResult
    voteResult      <- getByParams voteStr conn [memberID, topicID]
    return (MemberOnTopic allocatedSingle voteResult)
  where 
    allocStr = "SELECT * FROM AllocatedVote WHERE memberID=? AND topicID=? "
    voteStr  = "SELECT Vote.* FROM Vote "
               <> "JOIN Choice ON (Vote.choiceID=Choice.id) "
               <> "WHERE Vote.voterID=? AND Choice.topicID=? "
    
createMember :: Connection -> InMember -> EnvHandler (Only Integer)
createMember conn memb = do
    _ <- checkUsernameDNE conn memb
    dateJoined <- liftIO getCurrentTime
    let params = ( memb^.username
                 , dateJoined
                 , memb^.email
                 )
    single =<< liftIO (query conn qString params)
  where 
    qString = "INSERT INTO Member " 
              <> "(name, dateJoined, email) " 
              <> "VALUES (?,?,?) RETURNING *"

checkUsernameDNE :: Connection -> InMember -> EnvHandler ()
checkUsernameDNE conn member = do 
    let memberUsername = member^.username
    result <- liftIO (query conn "SELECT * FROM Member WHERE username=?);" [memberUsername])
    case result :: [Member] of 
        [] -> return ()
        _ -> throwError err404