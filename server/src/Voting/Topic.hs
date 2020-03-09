{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Voting.Topic (Topic, queryTopic, queryTopics) where
import Voting.Types
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


queryTopics :: Connection -> EnvHandler [Topic]
queryTopics conn = liftIO $ query_ conn "select * from Topic"
 
queryTopic :: Connection -> Integer -> EnvHandler Topic
queryTopic conn id = do    
    result <- liftIO (query conn "select * from Topic where id=?" [id])
    case result of 
        [topic] -> return topic
        _       -> throwError err404

queryTopicFull :: Connection -> Integer -> EnvHandler (Topic, [Choice])
queryTopicFull conn id = do
  topic <- queryTopic conn id
  let queryString = "SELECT * FROM Choice where topicID=?"
  choices <- liftIO (query conn queryString [id])
  return (topic, choices)

queryTopicVotes :: Connection -> Integer -> EnvHandler [Vote]
queryTopicVotes conn id = do    
    let queryString = ("SELECT Vote.* FROM Vote" 
                       <> "JOIN Choice ON (Vote.choiceID=Choice.name)"
                       <> "JOIN Topic ON (Choice.topicID=Topic.id)"
                       <> "WHERE Choice.topicID=?") 
    result <- liftIO (query conn queryString [id])
    return result

queryTopicUsers :: Connection -> Integer -> EnvHandler [(User, UserVote)]
queryTopicUsers conn id = return []
    