module View exposing (view)

import App exposing (..)
import Autocomplete
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json


view : Model -> Html Msg
view model =
    let
        options =
            { preventDefault = True, stopPropagation = False }

        dec =
            (Json.map
                (\code ->
                    if code == 38 || code == 40 then
                        Ok NoOp
                    else if code == 27 then
                        Ok HandleEscape
                    else
                        Err "not handling that key"
                )
                keyCode
            )
                |> Json.andThen
                    fromResult

        fromResult : Result String a -> Json.Decoder a
        fromResult result =
            case result of
                Ok val ->
                    Json.succeed val

                Err reason ->
                    Json.fail reason

        menu =
            if model.showMenu then
                [ viewMenu model ]
            else
                []

        query =
            case model.selectedPerson of
                Just person ->
                    person.name

                Nothing ->
                    model.query

        activeDescendant attributes =
            case model.selectedPerson of
                Just person ->
                    (attribute "aria-activedescendant"
                        person.name
                    )
                        :: attributes

                Nothing ->
                    attributes
    in
        div []
            (List.append
                [ input
                    (activeDescendant
                        [ onInput SetQuery
                        , onFocus OnFocus
                        , onWithOptions "keydown" options dec
                        , value query
                        , id "president-input"
                        , class "autocomplete-input"
                        , autocomplete False
                        , attribute "aria-owns" "list-of-presidents"
                        , attribute "aria-expanded" <| String.toLower <| toString model.showMenu
                        , attribute "aria-haspopup" <| String.toLower <| toString model.showMenu
                        , attribute "role" "combobox"
                        , attribute "aria-autocomplete" "list"
                        ]
                    )
                    []
                ]
                menu
            )


acceptablePeople : String -> List Person -> List Person
acceptablePeople query people =
    let
        lowerQuery =
            String.toLower query
    in
        List.filter (String.contains lowerQuery << String.toLower << .name) people


viewMenu : Model -> Html Msg
viewMenu model =
    div [ class "autocomplete-menu" ]
        [ Html.map SetAutoState (Autocomplete.view viewConfig model.howManyToShow model.autoState (acceptablePeople model.query model.people)) ]


viewConfig : Autocomplete.ViewConfig Person
viewConfig =
    let
        customizedLi keySelected mouseSelected person =
            { attributes =
                [ classList [ ( "autocomplete-item", True ), ( "key-selected", keySelected || mouseSelected ) ]
                , id person.name
                ]
            , children = [ Html.text person.name ]
            }
    in
        Autocomplete.viewConfig
            { toId = .name
            , ul = [ class "autocomplete-list" ]
            , li = customizedLi
            }



--div [ class "ea-site" ]
--    [ div [ class "ea-header" ]
--        [ div [ class "ea-header__content" ]
--            [ input
--                [ placeholder "What are you interested in?"
--                ]
--                []
--            ]
--        ]
--    , div [ class "ea-main" ]
--        [ div [ class "ea-header__content" ]
--            [ text "Main"
--            ]
--        ]
--    ]
