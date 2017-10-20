{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.SearchEngine
    ( insertDocs
    , invariant
    , queryExplain
    , overallScore
    )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Elmanac as API

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
import System.IO (hFlush, stdout)
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
import Elmify ((|>))

data RuntimeMode
    = Development
    | Production

data AppConfig =
    AppConfig
        { mode :: RuntimeMode
        }


repl :: IO ()
repl = do
    putStrLn ("reading catalog..." :: Text)
    docs <- API.initCatalog

    putStrLn ("forcing docs..." :: Text)
    evaluate (foldl' (\a p -> seq p a) () docs)

    let searchEngine = insertDocs docs API.initSimpleSearchEngine

    putStrLn ("constructing index..." :: Text)
    evaluate searchEngine >> return ()
    putStrLn $ "invariant" ++ show (invariant searchEngine)

--    print $ take 100 $ sortBy (flip compare) $ map Set.size $ Map.elems (termMap searchindex)
--    T.putStr $ T.unlines $ Map.keys (termMap searchindex)
--    let SearchEngine{searchIndex=SearchIndex{termMap, termIdMap, docKeyMap, docIdMap}} = searchengine
--    print (Map.size termMap, IntMap.size termIdMap, Map.size docKeyMap, IntMap.size docIdMap)

    let loop = do
          putStr ("search term>" :: Text)
          hFlush stdout
          t <- T.getLine
          --putStr $ "  searching for " ++ show t
          unless (T.null t) $ do
              putStrLn ("Ranked results:" :: Text)
              let rankedResults = queryExplain searchEngine (T.words t)

              putStr $ T.unlines
                  --[ show (overallScore explanation) -- ++ show ": " -- ++ API.displayName name
                  [ API.displayName name
                  | (explanation, name) <- take 10 rankedResults
                  ]

              loop
    return ()
    loop


app :: AppConfig -> ScottyM ()
app config = do
    middleware $ staticPolicy (noDots >-> addBase "assets") -- for favicon.ico
    middleware logStdoutDev
    middleware simpleCors

    get "/app.js" $ do
        redirect "/app-dev.js"

    get "/" $ do
        redirect "/index.html"

    --get "/some-json" $ do
    --    json $ API.garbage

    get "/api/v1/search/:term" $ do
        term <- param "term"
        json $ API.search term


main :: IO ()
main =
    --port <- read <$> getEnv "ELMANAC_PORT"
    --env <- read <$> getEnv "ELMANAC_ENV

    scotty 8080 $ app $ AppConfig Development
