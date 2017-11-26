module Projects.Init
    exposing
        ( init
        , initList
        )

import Dict as Dict
import Models exposing (Model, ProjectsModule, Project)
import Projects.Data exposing (allProjects)


init : ProjectsModule
init =
    { projectsActivated = Dict.empty }


initList : List Project
initList =
    allProjects
