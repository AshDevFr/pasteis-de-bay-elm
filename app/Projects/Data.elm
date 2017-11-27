module Projects.Data
    exposing
        ( allProjects
        )

import Projects.Model exposing (Project, ProjectCost)
import Projects.Msg exposing (Msg(..))


allProjects : List Project
allProjects =
    [ improvedAutoPasteis
    , begForMoreWire
    ]


improvedAutoPasteis : Project
improvedAutoPasteis =
    { id = "project1"
    , name = "Improved AutoPasteis"
    , description = "Increases AutoPasteis performance 25%"
    , trigger =
        (\model ->
            model.manufacturingModule.pasteisModule
                |> Maybe.map (\mod -> mod.level)
                |> Maybe.withDefault 0
                |> flip (>=) 1
        )
    , effect =
        [ MapOperations (flip (-) 750.0)
        , MapPasteisBoost (flip (+) 0.25)
        ]
    , cost = ProjectCost 0 750 0 0
    }


begForMoreWire : Project
begForMoreWire =
    { id = "projectButton2"
    , name = "Beg for More Wire"
    , description =
        "Admit failure, ask for budget increase to cover cost of 1 spool"
    , trigger =
        (always False)

    --  portTotal<wireCost && funds<wireCost && wire<1 && unsoldClips<1
    , effect =
        [ MapOperations (flip (-) 750.0)
        , MapTrust (flip (-) 100)
        ]
    , cost =
        ProjectCost 0 750 0 100
    }
