module Projects.Module.ProjectsModule exposing (ProjectsModule, ProjectsModuleSave)

import Dict exposing (Dict)


type alias ProjectsModule =
    { projectsActivated : Dict String Bool
    , projectsEnabled : Dict String Bool
    }


type alias ProjectsModuleSave =
    { projectsActivated : List ( String, Bool )
    }
