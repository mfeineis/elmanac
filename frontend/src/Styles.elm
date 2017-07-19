module Styles exposing (CssClasses(..), CssIds(..), css, helpers)

import Css exposing (..)
import Css.Elements exposing (body)
import Css.Namespace exposing (namespace)
import Html
import Html.CssHelpers exposing (Namespace)


type CssClasses
    = AutocompleteContainer
    | AutocompleteInput
    | AutocompleteMenu
    | AutocompleteList
    | AutocompleteItem
    | AutocompleteItemKeySelected


type CssIds
    = PresidentInput


appNamespace =
    "ea-"


css =
    (stylesheet << namespace appNamespace)
        [ body
            [ margin zero
            ]
        , id PresidentInput
            []
        , class AutocompleteContainer
            [ backgroundColor (hex "eee")
            ]
        , class AutocompleteInput
            [ borderRadius (px 3)
            ]
        , class AutocompleteMenu
            [ border3 (px 1) solid (hex "ddd")
            ]
        , class AutocompleteList
            []
        , class AutocompleteItem
            []
        , class AutocompleteItemKeySelected
            [ color (hex "f60")
            ]
        ]


helpers =
    let
        { id, class, classList } =
            Html.CssHelpers.withNamespace appNamespace
    in
    { cssId = id
    , cssClass = class
    , cssClassList = classList
    }
