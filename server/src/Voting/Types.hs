{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Voting.Types (Topic, Vote, AllocatedVote, Member, Condition, Choice) where
import Data.Aeson
import Data.Time
import GHC.Generics
import Database.PostgreSQL.Simple

data Topic = Topic
  { id :: Integer
  , name :: String  
  , description :: String
  , startTime :: UTCTime
  , endTime :: UTCTime
  } deriving (Eq, Show, Generic, FromRow, ToRow)
instance ToJSON Topic
instance FromJSON Topic

data Vote = Vote
  { voterID :: Integer
  , choiceID :: Integer
  , amount :: Integer
  , mostRecent :: UTCTime
  , comment :: String
  } deriving (Eq, Show, Generic, FromRow, ToRow)
instance ToJSON Vote
instance FromJSON Vote

data AllocatedVote = AllocatedVote 
  { memberID :: Integer
  , topicID :: Integer
  , votesAllocated :: Integer
  } deriving (Eq, Show, Generic, FromRow, ToRow)
instance ToJSON AllocatedVote
instance FromJSON AllocatedVote

data Member = Member
  { id :: Integer
  , username :: String  
  , dateJoined :: UTCTime
  , email :: Maybe String
  } deriving (Eq, Show, Generic, FromRow, ToRow)
instance ToJSON Member
instance FromJSON Member

data Condition = Condition
  { todo :: String
  } deriving (Eq, Show, Generic, FromRow, ToRow)
instance ToJSON Condition
instance FromJSON Condition

data Choice = Choice
  { id :: Integer
  , name :: String
  , topicID :: Integer
  , description :: String
  , proposedBy :: Integer
  , dateProposed :: UTCTime
  } deriving (Eq, Show, Generic, FromRow, ToRow)
instance ToJSON Choice
instance FromJSON Choice
