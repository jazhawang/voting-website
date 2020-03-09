{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Voting.Types (Topic, Vote, UserVote, User, Condition, Choice) where
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

data Topic = Topic
  { id :: Integer
  , name :: String  
  , description :: String
  , startTime :: Day
  , endTime :: Day
  } deriving (Eq, Show, Generic, FromRow, ToRow)
instance ToJSON Topic
instance FromJSON Topic

data Vote = Vote
  { voterID :: Integer
  , choiceID :: Integer
  , amount :: Integer
  , mostRecent :: Day
  , comment :: Maybe Day
  } deriving (Eq, Show, Generic, FromRow, ToRow)
instance ToJSON Vote
instance FromJSON Vote

data UserVote = UserVote 
  { userID :: Integer
  , topicID :: Integer
  , votesAllocated :: Integer
  } deriving (Eq, Show, Generic, FromRow, ToRow)
instance ToJSON UserVote
instance FromJSON UserVote

data User = User
  { id :: Integer
  , username :: String  
  , dateJoined :: Day
  , email :: String
  } deriving (Eq, Show, Generic, FromRow, ToRow)
instance ToJSON User
instance FromJSON User

data Condition = Condition
  { todo :: String
  } deriving (Eq, Show, Generic, FromRow, ToRow)
instance ToJSON Condition
instance FromJSON Condition

data Choice = Choice
  { name :: String
  , topicID :: Integer
  , description :: String
  , proposedBy :: Integer
  , dateProposed :: Day
  } deriving (Eq, Show, Generic, FromRow, ToRow)
instance ToJSON Choice
instance FromJSON Choice
