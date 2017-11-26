module Projects.Update
    exposing
        ( update
        )

import Html exposing (Html, text)
import Dict as Dict
import Projects.Msg as Projects exposing (..)
import Projects.Data exposing (allProjects)
import Projects.Model exposing (Project)
import Projects.Module.ProjectsModule exposing (ProjectsModule)


init : ProjectsModule
init =
    { projectsActivated = Dict.empty }


initList : List Project
initList =
    allProjects


update : Projects.Msg -> ProjectsModule -> ( ProjectsModule, Cmd msg )
update msg projectsModule =
    ( projectsModule, Cmd.none )
