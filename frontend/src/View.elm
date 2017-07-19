module View exposing (view)

import App exposing (..)
import Autocomplete
import Css
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Simple.Fuzzy as Fuzzy
import Styles exposing (CssClasses(..), CssIds(..))
import Util exposing ((=>))


{ cssId, cssClass, cssClassList } =
    Styles.helpers


eventOptions : { preventDefault : Bool, stopPropagation : Bool }
eventOptions =
    { preventDefault = True, stopPropagation = False }


fromResult : Result String a -> Json.Decoder a
fromResult result =
    case result of
        Ok val ->
            Json.succeed val

        Err reason ->
            Json.fail reason


decoder : Json.Decoder Msg
decoder =
    Json.map
        (\code ->
            if code == 38 || code == 40 then
                Ok NoOp
            else if code == 27 then
                Ok HandleEscape
            else
                Err "not handling that key"
        )
        keyCode
        |> Json.andThen
            fromResult


view : ViewOptions -> Model -> Html Msg
view options model =
    let
        menu =
            if model.showMenu then
                [ viewMenu model ]
            else
                []

        menuWithStyle =
            if options.injectStyles then
                menu
                    ++ [ Html.node "link"
                            [ href "../../assets/style.css"
                            , rel "stylesheet"
                            , type_ "text/css"
                            ]
                            []
                       ]
            else
                menu

        query =
            case model.selectedPerson of
                Just person ->
                    person.name

                Nothing ->
                    model.query

        activeDescendant attributes =
            case model.selectedPerson of
                Just person ->
                    attribute "aria-activedescendant"
                        person.name
                        :: attributes

                Nothing ->
                    attributes

        descendants =
            List.append
                [ input
                    (activeDescendant
                        [ onInput SetQuery
                        , onFocus OnFocus
                        , onWithOptions "keydown" eventOptions decoder
                        , value query
                        , cssId PresidentInput
                        , cssClass [ AutocompleteInput ]
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
                menuWithStyle
    in
    div [ cssClass [ AutocompleteContainer ] ]
        descendants


acceptablePeople : String -> List Person -> List Person
acceptablePeople query people =
    Fuzzy.filter .name query people


viewMenu : Model -> Html Msg
viewMenu model =
    let
        people =
            acceptablePeople model.query model.people

        view =
            Autocomplete.view viewConfig model.howManyToShow model.autoState people
    in
    div [ cssClass [ AutocompleteMenu ] ]
        [ Html.map SetAutoState view ]


viewConfig : Autocomplete.ViewConfig Person
viewConfig =
    let
        customizedLi keySelected mouseSelected person =
            { attributes =
                [ cssClassList
                    [ AutocompleteItem => True
                    , AutocompleteItemKeySelected => keySelected || mouseSelected
                    ]
                , id person.name
                ]
            , children = [ text person.name ]
            }
    in
    Autocomplete.viewConfig
        { toId = .name
        , ul = [ cssClass [ AutocompleteList ] ]
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
