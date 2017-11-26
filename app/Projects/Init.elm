module Projects.Init
    exposing
        ( init
        , initList
        , tryMakeProjectsModule
        )

import Dict as Dict
import Projects.Data exposing (allProjects)
import Projects.Model exposing (Project)
import Projects.Module.ProjectsModule exposing (ProjectsModule)
import Main.Model exposing (Model)


init : ProjectsModule
init =
    { projectsActivated = Dict.empty }


initList : List Project
initList =
    allProjects


tryMakeProjectsModule : Model -> Model
tryMakeProjectsModule model =
    model.computingModule
        |> Maybe.map (\mod -> model)
        |> Maybe.withDefault
            (let
                enoughPasteis =
                    model.pasteis >= 2000
             in
                case enoughPasteis of
                    False ->
                        model

                    True ->
                        { model | projectsModule = Just init }
            )