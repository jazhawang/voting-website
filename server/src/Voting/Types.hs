{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Voting.Types (
  Topic (..), Vote, AllocatedVote, Member, Condition, Choice, MemberAllocated,
  MemberOnTopic (..), FullTopic (..), InTopic (..), InMember (..)
  ) where

import Data.Aeson
import Data.Time
import GHC.Generics
import Database.PostgreSQL.Simple


-- TODO : 
-- A lot of these types are some similar to another types, plus or minus
-- so other fields. Find a way to instead describe these types through
-- (homomorphic?) transformations from on to the other.
data InTopic = InTopic
  { name :: String  
  , description :: Maybe String
  , proposedBy :: Integer  
  , endTime :: UTCTime
  } deriving (Eq, Show, Generic, FromRow, ToRow)
instance ToJSON InTopic
instance FromJSON InTopic

 
data Topic = Topic
  { id :: Integer
  , name :: String  
  , description :: Maybe String
  , proposedBy :: Integer
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

data InMember = InMember 
  { username :: String
  , email :: Maybe String
  } deriving (Eq, Show, Generic, FromRow, ToRow)
instance ToJSON InMember
instance FromJSON InMember

data Condition = Condition
  { todo :: String
  } deriving (Eq, Show, Generic, FromRow, ToRow)
instance ToJSON Condition
instance FromJSON Condition

data Choice = Choice
  { id :: Integer
  , name :: String
  , topicID :: Integer
  , description :: Maybe String
  , proposedBy :: Integer
  , dateProposed :: UTCTime
  } deriving (Eq, Show, Generic, FromRow, ToRow)
instance ToJSON Choice
instance FromJSON Choice


data MemberAllocated = MemberAllocated
  { memberID :: Integer
  , username :: String
  , email :: String
  , votesAllocated :: Integer
  } deriving (Eq, Show, Generic, FromRow, ToRow)
instance ToJSON MemberAllocated
instance FromJSON MemberAllocated

data FullTopic = FullTopic
  { id :: Integer
  , name :: String  
  , description :: Maybe String
  , proposedBy :: Integer
  , startTime :: UTCTime
  , endTime :: UTCTime
  , choices :: [Choice]
  } deriving (Eq, Show, Generic)
instance ToJSON FullTopic
instance FromJSON FullTopic

data MemberOnTopic = MemberOnTopic
  { allocated :: AllocatedVote
  , votes :: [Vote]
  } deriving (Eq, Show, Generic)
instance ToJSON MemberOnTopic
instance FromJSON MemberOnTopic