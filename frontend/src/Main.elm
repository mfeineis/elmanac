module Main exposing (main, reactor)

import App exposing (Model, Msg(..))
import Autocomplete
import Html
import Update
import View


type alias Flags =
    {}


defaultFlags : Flags
defaultFlags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { autoState = Autocomplete.empty
            , howManyToShow = 5
            , query = ""
            , people =
                [ { name = "Marty" }
                , { name = "Doc Brown" }
                , { name = "Einstein" }
                , { name = "The Libyens" }
                , { name = "Biff" }
                , { name = "Lorane" }
                , { name = "George" }
                ]
            , selectedPerson = Nothing
            , showMenu = False
            }
    in
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map SetAutoState Autocomplete.subscription


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , subscriptions = subscriptions
        , update = Update.update
        , view = View.view
        }


reactor : Program Never Model Msg
reactor =
    Html.program
        { init = init defaultFlags
        , subscriptions = subscriptions
        , update = Update.update
        , view = View.view
        }
