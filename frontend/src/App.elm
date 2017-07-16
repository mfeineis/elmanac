module App exposing (..)

import Autocomplete


type alias Model =
    { autoState : Autocomplete.State
    , howManyToShow : Int
    , query : String
    , people : List Person
    , selectedPerson : Maybe Person
    , showMenu : Bool
    }


type Msg
    = SetQuery String
    | SetAutoState Autocomplete.Msg
    | Wrap Bool
    | Reset
    | HandleEscape
    | SelectPersonKeyboard String
    | SelectPersonMouse String
    | PreviewPerson String
    | OnFocus
    | NoOp


type alias Person =
    { name : String
    }
