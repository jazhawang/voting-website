{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module AppEnv (AppEnv(..)) where
import GHC.Generics
import Database.PostgreSQL.Simple
import Data.Pool

{- Holds server resources and information for our handlers to use -}
newtype AppEnv = AppEnv {
  db :: Pool Connection  
} deriving (Generic)