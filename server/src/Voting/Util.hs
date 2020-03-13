
{-# LANGUAGE OverloadedStrings #-}

module Voting.Util (single, get, getSingleByID, getByID, getByParams) where

import Voting.Types
import Database.PostgreSQL.Simple
import EnvHandler
import Servant
import Control.Monad.IO.Class


single :: [a] -> EnvHandler a
single [x] = return x
single _   = throwError err404

get :: FromRow a => Query -> Connection -> EnvHandler [a]
get str conn = liftIO (query_ conn str)

getSingleByID :: FromRow a => Query -> Connection -> Integer -> EnvHandler a
getSingleByID queryStr conn id = single =<< getByID queryStr conn id

getByID :: FromRow a => Query -> Connection -> Integer -> EnvHandler [a]
getByID queryStr conn id = liftIO (query conn queryStr [id])

getByParams :: (ToRow b, FromRow a) => Query -> Connection -> b -> EnvHandler [a]
getByParams queryStr conn params = liftIO (query conn queryStr params)