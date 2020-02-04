module Main where

import Network.Wai.Handler.Warp
import GHC.IO.Encoding
import Database.PostgreSQL.Simple
import Data.Pool
import Configuration.Dotenv
import System.Envy
import Lib

main :: IO ()
main = do
    _ <- loadFile config
    dotenv <- decodeEnv :: IO (Either String ConnectInfo)    
    case dotenv of 
       Left errStr -> putStrLn errStr
       Right connectionInfo -> do
          setLocaleEncoding utf8
          let port = 8080
          putStrLn $ "✅  Running app on port: " ++ show port
          appEnv <- initAppEnv connectionInfo
          run port (app appEnv)


config :: Configuration.Dotenv.Config 
config = Config {
    configPath = ["./.env"]
  , configExamplePath = ["./.env.example"]
  , configOverride = False }


instance FromEnv ConnectInfo where
  fromEnv _ = 
    ConnectInfo <$> env "POSTGRES_HOST"
                <*> env "POSTGRES_PORT"
                <*> env "POSTGRES_USER"
                <*> env "POSTGRES_PASSWORD"
                <*> env "POSTGRES_DATABASE"

initAppEnv :: ConnectInfo -> IO AppEnv
initAppEnv info = do
  pool <- initConnectionPool info
  return (AppEnv {db=pool})

initConnectionPool :: ConnectInfo -> IO (Pool Connection)
initConnectionPool connInfo =
  createPool (connect connInfo)
             Database.PostgreSQL.Simple.close
             1 -- stripes
             60 -- how long unused connections kept for (seconds)
             2 -- connections per stripe

