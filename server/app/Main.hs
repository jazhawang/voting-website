module Main where

import Network.Wai.Handler.Warp
import GHC.IO.Encoding
import Lib

main :: IO ()
main = do
    setLocaleEncoding utf8
    let port = 8080
    putStrLn $ "✅  Running app on port: " ++ show port
    run port app
