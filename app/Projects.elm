module Projects
    exposing
        ( init
        , initList
        , update
        , view
        )

import Html exposing (Html, text)
import Dict as Dict
import Models exposing (Model, ProjectsModule, Project)
import Projects.Msg as Projects exposing (..)
import Projects.Data exposing (allProjects)


init : ProjectsModule
init =
    { projectsActivated = Dict.empty }


initList : List Project
initList =
    allProjects


update : Projects.Msg -> ProjectsModule -> ( ProjectsModule, Cmd msg )
update msg projectsModule =
    ( projectsModule, Cmd.none )


view : Model -> Html Models.Msg
view model =
    case model.computingModule of
        Nothing ->
            text ""

        Just mod ->
            text ""
