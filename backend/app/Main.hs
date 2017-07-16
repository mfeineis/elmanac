{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Elmanach as API

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


app :: ScottyM ()
app = do
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


runApp :: IO ()
runApp = scotty 8080 app


main :: IO ()
main = runApp
