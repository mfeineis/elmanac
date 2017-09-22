{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Elmanach
    ( SimpleDescription
    , SimpleSearchEngine
    , displayName
    , initCatalog
    , initSimpleSearchEngine
    , search
    )
  where

import Data.Aeson (Value(..), object, (.=))
--import qualified Data.Map as Map
import Data.Ix (Ix)
import Data.SearchEngine
    ( NoFeatures
    , SearchConfig(..)
    , SearchEngine
    , SearchRankParameters(..)
    , initSearchEngine
    , noFeatures
    )
import Data.Text (Text, toCaseFold)
import Protolude
--import Utils ((|>))

-- | Engine setup and initialization

type SimpleSearchEngine =
  SearchEngine SimpleDescription SimpleName SimpleField NoFeatures

type SimpleName = Text

data SimpleField
    = NameField
    | DescriptionField
  deriving (Eq, Ord, Enum, Bounded, Ix, Show)


data SimpleDescription =
    SimpleDescription
      { name :: Text
      , terms :: [Text]
      }

displayName :: SimpleName -> Text
displayName name =
    name

initSimpleSearchEngine :: SimpleSearchEngine
initSimpleSearchEngine =
    initSearchEngine simpleConfig defaultSearchRankParameters


simpleConfig :: SearchConfig SimpleDescription SimpleName SimpleField NoFeatures
simpleConfig =
    SearchConfig
        { documentKey = \(SimpleDescription { name }) -> name
        , extractDocumentTerms = extractSimpleDocumentTerms
        , transformQueryTerm = transformSimpleQueryTerm
        , documentFeatureValue = const noFeatures
        }
  where
    extractSimpleDocumentTerms :: SimpleDescription -> SimpleField -> [Text]
    extractSimpleDocumentTerms (SimpleDescription { terms }) field =
        terms

    transformSimpleQueryTerm :: Text -> SimpleField -> Text
    transformSimpleQueryTerm tok field =
        case field of
            NameField ->
                toCaseFold tok
                --"transformed name "

            DescriptionField ->
                toCaseFold tok
                --"transformed description"


defaultSearchRankParameters :: SearchRankParameters SimpleField NoFeatures
defaultSearchRankParameters =
    SearchRankParameters
        { paramK1
        , paramB
        , paramFieldWeights
        , paramFeatureWeights = noFeatures
        , paramFeatureFunctions = noFeatures
        , paramResultsetSoftLimit = 200
        , paramResultsetHardLimit = 400
        , paramAutosuggestPrefilterLimit  = 500
        , paramAutosuggestPostfilterLimit = 500
        }
  where
    paramK1 :: Float
    paramK1 = 1.5

    paramB :: SimpleField -> Float
    paramB NameField        = 0.9
    paramB DescriptionField = 0.5

    paramFieldWeights :: SimpleField -> Float
    paramFieldWeights NameField        = 20
    paramFieldWeights DescriptionField = 1


initCatalog :: IO [SimpleDescription]
initCatalog =
    return
        [ SimpleDescription "Blubb" ["Bla", "Plisch"]
        , SimpleDescription "Rot" ["Blau", "Gruen"]
        ]

search :: Text -> Value
search term =
    object
        [ "result" .= String term
        ]
