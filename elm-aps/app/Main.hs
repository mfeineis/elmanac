{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai (Application)
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
    , scotty
    , text
    )

import qualified Example as API


app :: ScottyM ()
app = do
    middleware $ staticPolicy (noDots >-> addBase "static/images") -- for favicon.ico
    middleware logStdoutDev

    get "/" $ do
        text "Hello World"

    --get "/some-json" $ do
    --    json $ API.garbage

    get "/search/:term" $ do
        term <- param "term"
        json $ API.search term


runApp :: IO ()
runApp = scotty 8080 app


main :: IO ()
main = runApp
