{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Example (initCatalog, search) where

import Data.Aeson (Value(..), object, (.=))
--import qualified Data.Map as Map
--import Data.SearchEngine
import Data.Text (Text)
import Protolude
import Utils ((|>))


initCatalog :: ()
initCatalog = ()
-- readPackages :: IO [PackageDescription]
-- readPackages = do
--   exists <- doesFileExist "00-index.tar"
--   when (not exists) $ do
--     putStrLn "This program needs a 00-index.tar package index."
--     putStrLn "Please grab 00-index.tar.gz from hackage and gunzip it."
--     exitFailure
--
--   pkgs <- PackageIndexUtils.readPackageIndexFile "00-index.tar"
--   let latestPkgs = Map.fromListWith
--                      (\a b -> if packageVersion (fst a) > packageVersion (fst b)
--                                 then a else b)
--                      [ (packageName pkgid, (pkgid, pkg))
--                      | (pkgid, pkg) <- pkgs ]
--
--   return . map (flattenPackageDescription . snd)
--          . Map.elems
--          $ latestPkgs


search :: Text -> Value
search term =
    object
        [ "result" .= String term
        ]
