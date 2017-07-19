module Update exposing (update)

import App exposing (..)
import Autocomplete
import Dom
import Simple.Fuzzy as Fuzzy
import Task


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetQuery newQuery ->
            let
                showMenu =
                    not << List.isEmpty <| acceptablePeople newQuery model.people
            in
            { model | query = newQuery, showMenu = showMenu, selectedPerson = Nothing } ! []

        SetAutoState autoMsg ->
            let
                people =
                    acceptablePeople model.query model.people

                ( newState, maybeMsg ) =
                    Autocomplete.update updateConfig autoMsg model.howManyToShow model.autoState people

                newModel =
                    { model | autoState = newState }
            in
            case maybeMsg of
                Nothing ->
                    newModel ! []

                Just updateMsg ->
                    update updateMsg newModel

        HandleEscape ->
            let
                people =
                    acceptablePeople model.query model.people

                validOptions =
                    not <| List.isEmpty people

                handleEscape =
                    if validOptions then
                        model
                            |> removeSelection
                            |> resetMenu
                    else
                        model
                            |> resetInput

                escapedModel =
                    case model.selectedPerson of
                        Just person ->
                            if model.query == person.name then
                                model
                                    |> resetInput
                            else
                                handleEscape

                        Nothing ->
                            handleEscape
            in
            escapedModel ! []

        Wrap toTop ->
            case model.selectedPerson of
                Just person ->
                    update Reset model

                Nothing ->
                    let
                        people =
                            acceptablePeople model.query model.people
                    in
                    if toTop then
                        { model
                            | autoState = Autocomplete.resetToLastItem updateConfig people model.howManyToShow model.autoState
                            , selectedPerson = List.head <| List.reverse <| List.take model.howManyToShow <| acceptablePeople model.query model.people
                        }
                            ! []
                    else
                        { model
                            | autoState = Autocomplete.resetToFirstItem updateConfig (acceptablePeople model.query model.people) model.howManyToShow model.autoState
                            , selectedPerson = List.head <| List.take model.howManyToShow <| acceptablePeople model.query model.people
                        }
                            ! []

        Reset ->
            { model | autoState = Autocomplete.reset updateConfig model.autoState, selectedPerson = Nothing } ! []

        SelectPersonKeyboard id ->
            let
                newModel =
                    setQuery model id
                        |> resetMenu
            in
            newModel ! []

        SelectPersonMouse id ->
            let
                newModel =
                    setQuery model id
                        |> resetMenu
            in
            ( newModel, Task.attempt (\_ -> NoOp) (Dom.focus "president-input") )

        PreviewPerson id ->
            { model | selectedPerson = Just <| getPersonAtId model.people id } ! []

        OnFocus ->
            model ! []

        NoOp ->
            model ! []


acceptablePeople : String -> List Person -> List Person
acceptablePeople query people =
    Fuzzy.filter .name query people


updateConfig : Autocomplete.UpdateConfig Msg Person
updateConfig =
    Autocomplete.updateConfig
        { toId = .name
        , onKeyDown =
            \code maybeId ->
                if code == 38 || code == 40 then
                    Maybe.map PreviewPerson maybeId
                else if code == 13 then
                    Maybe.map SelectPersonKeyboard maybeId
                else
                    Just <| Reset
        , onTooLow = Just <| Wrap False
        , onTooHigh = Just <| Wrap True
        , onMouseEnter = \id -> Just <| PreviewPerson id
        , onMouseLeave = \_ -> Nothing
        , onMouseClick = \id -> Just <| SelectPersonMouse id
        , separateSelections = False
        }


resetInput model =
    { model | query = "" }
        |> removeSelection
        |> resetMenu


removeSelection model =
    { model | selectedPerson = Nothing }


getPersonAtId people id =
    List.filter (\x -> x.name == id) people
        |> List.head
        |> Maybe.withDefault (Person "")


setQuery model id =
    { model
        | query = .name <| getPersonAtId model.people id
        , selectedPerson = Just <| getPersonAtId model.people id
    }


resetMenu model =
    { model
        | autoState = Autocomplete.empty
        , showMenu = False
    }
