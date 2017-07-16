module Main exposing (main, reactor)

import Html exposing (Html)


type alias ProgramFlags =
    {}


defaultFlags : ProgramFlags
defaultFlags =
    {}


type alias Model =
    {}


type Msg
    = NoOp


init : ProgramFlags -> ( Model, Cmd Msg )
init flags =
    ( {}, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.text "Hello Elm!"


main : Program ProgramFlags Model Msg
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
