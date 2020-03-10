{-# LANGUAGE OverloadedStrings #-}

module Voting.Topic (Topic, queryTopic, queryTopics) where
import Voting.Types
import Database.PostgreSQL.Simple
import EnvHandler
import Servant
import Control.Monad.IO.Class


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
    let queryString = "SELECT Vote.* FROM Vote" 
                      <> "JOIN Choice ON (Vote.choiceID=Choice.name)"
                      <> "JOIN Topic ON (Choice.topicID=Topic.id)"
                      <> "WHERE Choice.topicID=?"
    liftIO (query conn queryString [id])

queryTopicUsers :: Connection -> Integer -> EnvHandler [(User, UserVote)]
queryTopicUsers conn id = throwError err404

createTopic :: Connection -> Topic -> [UserVote] -> EnvHandler ()
createTopic conn topic userVotes = throwError err404

updateTopic :: Connection -> Integer -> Topic -> EnvHandler ()
updateTopic conn id topic = throwError err404

deleteTopic :: Connection -> Integer -> EnvHandler ()
deleteTopic conn id = throwError err404
