module Main exposing (main, reactor)

import Html exposing (Html)
import Html.Attributes as Attr
import Styled


-- App setup


type alias Model =
    {}


type Msg
    = NoOp


type alias Flags =
    {}


defaultFlags : Flags
defaultFlags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( {}, Cmd.none )


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


reactor : Program Never Model Msg
reactor =
    Html.program
        { init = init defaultFlags
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- Staying in contact in the world


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- State management


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- Displaying the current state


view : Model -> Html Msg
view model =
    Styled.pageFrame []
        [ Styled.pageHeader []
            [ Styled.pageLogo []
                [ Html.text "Elm"
                ]
            , Styled.mainSearchInput
                [ Attr.placeholder "Do you want to know more?"
                ]
                []
            ]
        , Styled.pageMain
            [ Styled.topicContainer (Html.text "Booleans")
                []
                [ Styled.topicItem [] [ Html.text "Blubb" ]
                , Styled.topicItem [] [ Html.text "Bla" ]
                , Styled.topicItem [] [ Html.text "Plisch" ]
                ]
            , Styled.topicContainer (Html.text "Numbers")
                []
                [ Styled.topicItem [] [ Html.text "Int" ]
                , Styled.topicItem [] [ Html.text "Float" ]
                , Styled.topicItem [] [ Html.text "Double" ]
                , Styled.topicItem [] [ Html.text "Complex" ]
                ]
            , Styled.topicContainer (Html.text "Maybe")
                []
                [ Styled.topicItem [] [ Html.text "Nothing" ]
                , Styled.topicItem [] [ Html.text "Just something" ]
                ]
            ]
            [ settingsView model
            ]
        , Styled.pageSecondary []
            [ Html.text "Red"
            ]
        , Styled.pageExtra []
            [ Html.text "Green"
            ]
        , Styled.pageFooter []
            [ Styled.sectionTitle [] [ Html.text "About Elmanach" ]
            , Styled.sectionContent []
                [ Html.text
                    """
                    Elmanach strives to be a comprehensive information hub
                    for the Elm ecosystem.
                    """
                ]
            ]
        ]


settingsView : Model -> Html Msg
settingsView model =
    Styled.detailContainer []
        [ Styled.detailTitle [] [ Html.text "Settings" ]
        , Styled.detailContent []
            [ Html.text
                """
                Here you may choose the settings for your
                personal Elmanach experience.
                """
            ]
        , Styled.detailContent []
            [ Styled.detailCheckbox []
                [ Html.text "I am an Elm developer"
                ]
            , Styled.detailList []
                [ Styled.detailCheckbox [ Attr.checked True ]
                    [ Html.text "Enabled Code Search" ]
                , Styled.detailCheckbox [ Attr.checked True ]
                    [ Html.text "Include Doc comments" ]
                , Styled.detailCheckbox []
                    [ Html.text "Include type signatures" ]
                , Styled.detailCheckbox []
                    [ Html.text "Include migration topics" ]
                ]
            ]
        , Styled.detailContent []
            [ Styled.detailCheckbox []
                [ Html.text "I am new to web development"
                ]
            , Styled.detailList []
                [ Styled.detailCheckbox [ Attr.checked True ]
                    [ Html.text "Prioritize basic constructs" ]
                , Styled.detailCheckbox [ Attr.checked True ]
                    [ Html.text "Show \"Getting Started\" content" ]
                ]
            ]
        , Styled.detailContent []
            [ Styled.detailCheckbox []
                [ Html.text "I am a manager or CTO"
                ]
            , Styled.detailList []
                [ Styled.detailCheckbox [ Attr.checked True ]
                    [ Html.text "Include Roadmap" ]
                , Styled.detailCheckbox [ Attr.checked True ]
                    [ Html.text "Include non-technical sources" ]
                ]
            ]
        , Styled.detailContent []
            [ Html.text "Your settings will be safed in `localStorage`." ]
        , Styled.detailCheckbox
            [ Attr.checked True ]
            [ Html.text "Save Settings" ]
        ]
