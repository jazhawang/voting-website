{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances #-}

module Voting.Types where

import Data.Aeson
import Data.Time
import GHC.Generics
import Database.PostgreSQL.Simple
import Control.Lens hiding (Choice)


data InTopic = InTopic
  { _inTopicName :: String  
  , _inTopicDescription :: Maybe String
  , _inTopicProposedBy :: Integer  
  , _inTopicEndTime :: UTCTime
  } deriving (Eq, Show, Generic, FromRow, ToRow)
instance ToJSON InTopic
instance FromJSON InTopic
makeFields ''InTopic

data Topic = Topic
  { _topicId :: Integer
  , _topicName :: String  
  , _topicDescription :: Maybe String
  , _topicProposedBy :: Integer
  , _topicStartTime :: UTCTime
  , _topicEndTime :: UTCTime
  } deriving (Eq, Show, Generic, FromRow, ToRow)
instance ToJSON Topic
instance FromJSON Topic
makeFields ''Topic

data Vote = Vote
  { _voteVoterID :: Integer
  , _voteChoiceID :: Integer
  , _voteAmount :: Integer
  , _voteMostRecent :: UTCTime
  , _voteComment :: String
  } deriving (Eq, Show, Generic, FromRow, ToRow)
instance ToJSON Vote
instance FromJSON Vote
makeFields ''Vote

data AllocatedVote = AllocatedVote
  { _allocatedVoteMemberID :: Integer
  , _allocatedVoteTopicID :: Integer
  , _allocatedVoteVotesAllocated :: Integer
  } deriving (Eq, Show, Generic, FromRow, ToRow)
instance ToJSON AllocatedVote
instance FromJSON AllocatedVote
makeFields ''AllocatedVote

data Member = Member
  { _memberId :: Integer
  , _memberUsername :: String  
  , _memberDateJoined :: UTCTime
  , _memberEmail :: Maybe String
  } deriving (Eq, Show, Generic, FromRow, ToRow)
instance ToJSON Member
instance FromJSON Member
makeFields ''Member

data InMember = InMember 
  { _inMemberUsername :: String
  , _inMemberEmail :: Maybe String
  } deriving (Eq, Show, Generic, FromRow, ToRow)
instance ToJSON InMember
instance FromJSON InMember
makeFields ''InMember


data Condition = Condition
  { _conditionTodo :: String
  } deriving (Eq, Show, Generic, FromRow, ToRow)
instance ToJSON Condition
instance FromJSON Condition
makeFields ''Condition


data Choice = Choice
  { _choiceId :: Integer
  , _choiceName :: String
  , _choiceTopicID :: Integer
  , _choiceDescription :: Maybe String
  , _choiceProposedBy :: Integer
  , _choiceDateProposed :: UTCTime
  } deriving (Eq, Show, Generic, FromRow, ToRow)
instance ToJSON Choice
instance FromJSON Choice
makeFields ''Choice

data MemberAllocated = MemberAllocated
  { _memberAllocatedMemberID :: Integer
  , _memberAllocatedUsername :: String
  , _memberAllocatedEmail :: String
  , _memberAllocatedVotesAllocated :: Integer
  } deriving (Eq, Show, Generic, FromRow, ToRow)
instance ToJSON MemberAllocated
instance FromJSON MemberAllocated
makeFields ''MemberAllocated

data FullTopic = FullTopic
  { _fullTopicId :: Integer
  , _fullTopicName :: String  
  , _fullTopicDescription :: Maybe String
  , _fullTopicProposedBy :: Integer
  , _fullTopicStartTime :: UTCTime
  , _fullTopicEndTime :: UTCTime
  , _fullTopicChoices :: [Choice]
  } deriving (Eq, Show, Generic)
instance ToJSON FullTopic
instance FromJSON FullTopic
makeFields ''FullTopic

data MemberOnTopic = MemberOnTopic
  { _memberOnTopicAllocated :: AllocatedVote
  , _memberOnTopicAllocatedVotes :: [Vote]
  } deriving (Eq, Show, Generic)
instance ToJSON MemberOnTopic
instance FromJSON MemberOnTopic
makeFields ''MemberOnTopic
