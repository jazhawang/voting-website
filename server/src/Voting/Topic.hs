{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Voting.Topic 
  ( queryTopic
  , queryTopics
  , queryTopicFull
  , queryTopicVotes
  , queryTopicMembers
  , createTopic
  ) where

import Prelude hiding (id)
import Voting.Types
import Voting.Util
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import EnvHandler
import Servant
import Data.Time
import Data.Time.Clock
import Control.Monad.IO.Class
import Control.Lens hiding (Choice)

queryTopics :: Connection -> EnvHandler [Topic]
queryTopics = get "SELECT * FROM Topic"
 
queryTopic :: Connection -> Integer -> EnvHandler Topic
queryTopic = getSingleByID  "SELECT * FROM Topic WHERE id=?"

createFullTopic :: Topic -> [Choice] -> FullTopic
createFullTopic t choices =
  FullTopic (t^.id)
            (t^.name)
            (t^.description)
            (t^.proposedBy)
            (t^.startTime)
            (t^.endTime)
            choices

queryTopicFull :: Connection -> Integer -> EnvHandler FullTopic
queryTopicFull conn id = do
  topic   <- queryTopic conn id
  choices <- getByID "SELECT * FROM Choice where topicID=?" conn id
  return (createFullTopic topic choices)

queryTopicVotes :: Connection -> Integer -> EnvHandler [Vote]
queryTopicVotes = getByID queryString 
  where queryString = "SELECT Vote.* FROM Vote " 
                      <> "JOIN Choice ON (Vote.choiceID=Choice.id) "
                      <> "WHERE Choice.topicID=?"

queryTopicMembers :: Connection -> Integer -> EnvHandler [MemberAllocated]
queryTopicMembers = getByID queryString
  where queryString = "SELECT Member.id, Member.username, Member.email, " 
                      <> "AllocatedVote.votesAllocated FROM Member "
                      <> "JOIN AllocatedVote ON (Member.id=AllocatedVote.memberID) "
                      <> "WHERE AllocatedVote.topicID=?"


createTopic :: Connection -> InTopic -> EnvHandler Topic
createTopic conn topic = do
  logServer "Processing POST request"
  -- make sure name isn't used by an active topic
  _     <- checkName conn topic
  -- make sure ending date has not happened yet
  start <- checkDate topic
  let params = ( topic^.name
               , topic^.description
               , topic^.proposedBy
               , start
               , topic^.endTime
               )
  single =<< liftIO (query conn qString params)
  where 
    qString = "INSERT INTO Topic " 
              <> "(name, description, proposedBy, startTime, endTime) " 
              <> "VALUES (?,?,?,?,?) RETURNING *"

  
-- check that the given end time is before the current time, return startTime
checkDate :: InTopic -> EnvHandler UTCTime
checkDate topic = do
  let end = topic^.endTime
  start <- liftIO getCurrentTime  
  if start `before` end then
    return start
  else throwError err404

-- check that the given topic name isnt already in user by an active topic
checkName :: Connection -> InTopic -> EnvHandler ()
checkName conn topic = do
  let topicName = topic^.name
  current <- liftIO getCurrentTime
  endTimes <- liftIO $ sameNameTopicEndTimes conn topicName
  if any (before current) endTimes then
    throwError err404
  else return ()

sameNameTopicEndTimes :: Connection -> String -> IO [UTCTime]
sameNameTopicEndTimes conn name = do 
  result <- query conn "SELECT endTime FROM Topic WHERE name=?" [name]
  return (fromOnly <$> result)


updateTopic :: Connection -> Integer -> Topic -> EnvHandler ()
updateTopic conn id topic = throwError err404

deleteTopic :: Connection -> Integer -> EnvHandler ()
deleteTopic conn id = throwError err404
