{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Voting.Types where

import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Types
import Data.Aeson.TH
import Data.Time
import GHC.Generics
import Database.PostgreSQL.Simple
import qualified Data.Text as T
import Control.Lens hiding (Choice)


data InTopic = InTopic
  { _inTopicName :: String  
  , _inTopicDescription :: Maybe String
  , _inTopicProposedBy :: Integer  
  , _inTopicEndTime :: UTCTime
  } deriving (Eq, Show, Generic, FromRow, ToRow)

$(deriveJSON
    defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop (T.length "_inTopic")
        } ''InTopic)

$(makeFields ''InTopic)

data Topic = Topic
  { _topicId :: Integer
  , _topicName :: String  
  , _topicDescription :: Maybe String
  , _topicProposedBy :: Integer
  , _topicStartTime :: UTCTime
  , _topicEndTime :: UTCTime
  } deriving (Eq, Show, Generic, FromRow, ToRow)

$(deriveJSON
    defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop (T.length "_topic")
        } ''Topic)

$(makeFields ''Topic)

data Vote = Vote
  { _voteVoterID :: Integer
  , _voteChoiceID :: Integer
  , _voteAmount :: Integer
  , _voteMostRecent :: UTCTime
  , _voteComment :: String
  } deriving (Eq, Show, Generic, FromRow, ToRow)

type MemberVotes = [Vote]

$(deriveJSON
    defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop (T.length "_vote")
        } ''Vote)

$(makeFields ''Vote)

data AllocatedVote = AllocatedVote
  { _allocatedVoteMemberID :: Integer
  , _allocatedVoteTopicID :: Integer
  , _allocatedVoteVotesAllocated :: Integer
  } deriving (Eq, Show, Generic, FromRow, ToRow)

$(deriveJSON
    defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop (T.length "_allocatedVote")
        } ''AllocatedVote)

$(makeFields ''AllocatedVote)

data Member = Member
  { _memberId :: Integer
  , _memberUsername :: String  
  , _memberDateJoined :: UTCTime
  , _memberEmail :: Maybe String
  } deriving (Eq, Show, Generic, FromRow, ToRow)

$(deriveJSON
    defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop (T.length "_member")
        } ''Member)

$(makeFields ''Member)

data InMember = InMember 
  { _inMemberUsername :: String
  , _inMemberEmail :: Maybe String
  } deriving (Eq, Show, Generic, FromRow, ToRow)

$(deriveJSON
    defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop (T.length "_inMember")
        } ''InMember)

$(makeFields ''InMember)


data Condition = Condition
  { _conditionTodo :: String
  } deriving (Eq, Show, Generic, FromRow, ToRow)

$(deriveJSON
    defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop (T.length "_Condition")
        } ''Condition)

$(makeFields ''Condition)


data InChoice = InChoice
  { _inChoiceName :: String
  , _inChoiceTopicID :: Integer
  , _inChoiceDescription :: Maybe String
  , _inChoiceProposedBy :: Integer  
  } deriving (Eq, Show, Generic, FromRow, ToRow)

$(deriveJSON
    defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop (T.length "_inChoice")
        } ''InChoice)

$(makeFields ''InChoice)

data Choice = Choice
  { _choiceId :: Integer
  , _choiceName :: String
  , _choiceTopicID :: Integer
  , _choiceDescription :: Maybe String
  , _choiceProposedBy :: Integer
  , _choiceDateProposed :: UTCTime
  } deriving (Eq, Show, Generic, FromRow, ToRow)

$(deriveJSON
    defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop (T.length "_choice")
        } ''Choice)

$(makeFields ''Choice)


-- used to output data for an endpoint
data MemberAllocated = MemberAllocated
  { _memberAllocatedMemberID :: Integer
  , _memberAllocatedUsername :: String
  , _memberAllocatedEmail :: String
  , _memberAllocatedVotesAllocated :: Integer
  } deriving (Eq, Show, Generic, FromRow, ToRow)

$(deriveJSON
    defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop (T.length "_memberAllocated")
        } ''MemberAllocated)

$(makeFields ''MemberAllocated)


data FullTopic = FullTopic
  { _fullTopicId :: Integer
  , _fullTopicName :: String  
  , _fullTopicDescription :: Maybe String
  , _fullTopicProposedBy :: Integer
  , _fullTopicStartTime :: UTCTime
  , _fullTopicEndTime :: UTCTime
  , _fullTopicChoices :: [Choice]
  } deriving (Eq, Show, Generic)

$(deriveJSON
    defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop (T.length "_fullTopic")
        } ''FullTopic)

$(makeFields ''FullTopic)


data MemberOnTopic = MemberOnTopic
  { _memberOnTopicAllocated :: AllocatedVote
  , _memberOnTopicAllocatedVotes :: [Vote]
  } deriving (Eq, Show, Generic)

$(deriveJSON
    defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop (T.length "_memberOnTopic")
        } ''MemberOnTopic)

$(makeFields ''MemberOnTopic)