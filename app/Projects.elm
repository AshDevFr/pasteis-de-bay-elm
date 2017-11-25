module Projects
    exposing
        ( init
        , initList
        , update
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


update : Projects.Msg -> ProjectsModule -> ( ProjectsModule, Cmd msg )
update msg projectsModule =
    ( projectsModule, Cmd.none )


view : Model -> Html Models.Msg
view model =
    case model.projectsModule of
        Nothing ->
            text ""

        Just mod ->
            text "Pwet"
