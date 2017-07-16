{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Main (main) where

import Data.Aeson (Object, Value(..))
import Data.Text (Text, pack)
import GHC.Exts (fromList)
--import Network.HTTP.Types.Header
import Test.Hspec (describe, hspec, it, shouldBe)
--import Test.Hspec.Wai
--import Test.Hspec.Wai.JSON
import Test.QuickCheck (property)

import qualified Example as SUT

main :: IO ()
main = hspec $ do
    describe "elm-aps/search" $ do
        it "Stupid property test" $
            "s" `shouldBe` "s"

        it "Just returns the search term wrapped in a JSON result" $
            property $
                \term -> SUT.search (pack term) ==
                    (Object $ fromList [ ( "result", String (pack term) ) ])

                -- \x -> (read . show) x == (x :: Int)
                -- \term -> term `shouldBe` (term :: String)
                -- \term -> term `shouldBe` (term :: Text)
--spec :: Spec
--spec = with app $ do
--  describe "GET /" $ do
--    it "responds with 200" $ do
--      get "/" `shouldRespondWith` 200
--
--    it "responds with 'hello'" $ do
--      get "/" `shouldRespondWith` "hello"
--
--    it "responds with 200 / 'hello'" $ do
--      get "/" `shouldRespondWith` "hello" {matchStatus = 200}
--
--    it "has 'Content-Type: text/plain; charset=utf-8'" $ do
--      get "/" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"]}
--
--  describe "GET /some-json" $ do
--    it "responds with some JSON" $ do
--      get "/some-json" `shouldRespondWith` expectedJsonResponse
--
--expectedJsonResponse =
--  let ResponseMatcher status headers body = [json|{foo: 23, bar: 42}|]
--  in ResponseMatcher status [hContentType <:> "application/json; charset=utf-8"] body
