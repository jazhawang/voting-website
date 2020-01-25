module Main where

import Network.Wai.Handler.Warp
import Lib

main :: IO ()
main = do
    let port = 8080
    putStrLn $ "✅  Running app on port: " ++ show port
    run port app
