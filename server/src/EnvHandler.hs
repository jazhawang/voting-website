{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module EnvHandler (EnvHandler, envHandlerToHandler) where
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Servant
import Servant.Server
import Data.Aeson
import Data.Aeson.Types
import Data.Time.Calendar
import GHC.Generics
import Database.PostgreSQL.Simple
import Data.Pool
import AppEnv

{- Handler type which also allows reading global resources -}
type EnvHandler = ReaderT AppEnv Handler

{- Natural monad morphism from (AppEnv, EnvHandler) to Handler, we need this
   because Servant expects its own Handler type, but we need to add Reader
   functionality to it so we can read from resources like database pools -}
envHandlerToHandler :: AppEnv -> EnvHandler a -> Handler a
envHandlerToHandler state envHandler = runReaderT envHandler state
