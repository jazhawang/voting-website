{-# LANGUAGE OverloadedStrings #-}

module Voting.Topic 
  ( queryTopic
  , queryTopics
  , queryTopicFull
  , queryTopicVotes
  , queryTopicMembers) where

import Voting.Types
import Voting.Util
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import EnvHandler
import Servant
import Control.Monad.IO.Class


queryTopics :: Connection -> EnvHandler [Topic]
queryTopics = get "SELECT * FROM Topic"
 
queryTopic :: Connection -> Integer -> EnvHandler Topic
queryTopic = getSingleByID  "SELECT * FROM Topic WHERE id=?"

queryTopicFull :: Connection -> Integer -> EnvHandler (Topic, [Choice])
queryTopicFull conn id = do
  topic   <- queryTopic conn id
  choices <- getByID "SELECT * FROM Choice where topicID=?" conn id
  return (topic, choices)

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


createTopic :: Connection -> Topic -> [AllocatedVote] -> EnvHandler ()
createTopic conn topic allocatedVotes = throwError err404

updateTopic :: Connection -> Integer -> Topic -> EnvHandler ()
updateTopic conn id topic = throwError err404

deleteTopic :: Connection -> Integer -> EnvHandler ()
deleteTopic conn id = throwError err404
