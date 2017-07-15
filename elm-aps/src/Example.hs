{-# LANGUAGE OverloadedStrings #-}
module Example (garbage, search) where

import Data.Aeson (Value(..), object, (.=))
import Data.Text (Text)
import Utils ((|>))

garbage :: Value
garbage =
    object
        [ "foo" .= Number 23
        , "bar" .= Number 42
        ]


search :: Text -> Value
search term =
    object
        [ "result" .= String term
        ]
