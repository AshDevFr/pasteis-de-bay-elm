module Projects.Init
    exposing
        ( init
        , initList
        , activeList
        , inactiveList
        , tryMakeProjectsModule
        )

import Dict as Dict
import Projects.Data exposing (allProjects)
import Projects.Model exposing (Project)
import Projects.Module.ProjectsModule exposing (ProjectsModule)
import Main.Model exposing (Model)


init : ProjectsModule
init =
    { projectsActivated = Dict.empty
    , projectsEnabled = Dict.empty
    }


initList : ProjectsModule -> List Project
initList projectsModule =
    List.filter
        (\p ->
            not (Dict.get p.id projectsModule.projectsActivated == Just True)
        )
        allProjects


activeList : ProjectsModule -> List Project
activeList projectsModule =
    List.filter
        (\p ->
            (Dict.get p.id projectsModule.projectsEnabled == Just True)
        )
        (initList projectsModule)


inactiveList : ProjectsModule -> List Project
inactiveList projectsModule =
    List.filter
        (\p ->
            not (Dict.get p.id projectsModule.projectsEnabled == Just True)
        )
        (initList projectsModule)


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
