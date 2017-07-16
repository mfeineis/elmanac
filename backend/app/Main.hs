{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Elmanach as API

--import Control.Applicative ((<$>))
import Network.Wai (Application)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static
    ( addBase
    , noDots
    , staticPolicy
    , (>->)
    )
import Protolude hiding (get)
--import System.Environment (getEnv)
import Web.Scotty
    ( ScottyM
    , get
    , json
    , middleware
    , param
    , redirect
    , scotty
    , text
    )

data RuntimeMode
    = Development
    | Production

data AppConfig =
    AppConfig
        { mode :: RuntimeMode
        }

app :: AppConfig -> ScottyM ()
app config = do
    middleware $ staticPolicy (noDots >-> addBase "assets") -- for favicon.ico
    middleware logStdoutDev
    middleware simpleCors

    get "/" $ do
        redirect "/index.html"

    --get "/some-json" $ do
    --    json $ API.garbage

    get "/api/v1/search/:term" $ do
        term <- param "term"
        json $ API.search term


main :: IO ()
main =
    --port <- read <$> getEnv "ELMANACH_PORT"
    --env <- read <$> getEnv "ELMANACH_ENV

    scotty 8080 $ app $ AppConfig Development
