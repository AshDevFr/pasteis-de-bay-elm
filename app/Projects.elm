module Projects
    exposing
        ( init
        , initList
        , view
        )

import Html exposing (Html, text)
import Dict as Dict
import Models exposing (..)
import Projects.Msg as Projects exposing (..)


init : ProjectsModule
init =
    { projectsActivated = Dict.empty
    , projects = initList
    }


initList : List Project
initList =
    []


view : Model -> Html Models.Msg
view model =
    case model.projectsModule of
        Nothing ->
            text ""

        Just mod ->
            text "Pwet"
