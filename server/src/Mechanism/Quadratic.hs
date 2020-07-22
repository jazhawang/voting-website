{- Functions related to Quadratic Voting Mechanism -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mechanism.Quadratic 
    ( getTotalVotes
    , getQuadraticWeight
    ) where

import Voting.Types
import Control.Lens hiding (Choice)

type Weight = Double
type ChoiceId = Integer
type UserId = Integer

getTotalVotes :: [Vote] -> Integer
getTotalVotes votes = sum (map _voteAmount votes)

getQuadraticWeight :: [Vote] -> Weight
getQuadraticWeight votes = sqrt . fromIntegral $ getTotalVotes votes

groupByMemberAndTopic :: UserVotes -> [(ChoiceId, Weight)]
groupByMemberAndTopic _ = [] -- TODO

sortQuadratic :: [UserVotes] -> [(ChoiceId, Weight)]
--sortQuadratic userVotes = 
--    groupOn fst (concat (map groupByMemberAndTopic userVotes))


groupTuple :: Eq a => [(a,b)] -> [(a, [b])]
groupTuple tups = groupSort tups -- not the most efficient

getWinner :: [UserVotes] -> ChoiceId
getWinner votes = (sortQuadratic votes) !! 0