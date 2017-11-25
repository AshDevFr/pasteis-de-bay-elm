module Projects.Data
    exposing
        ( allProjects
        )

import Models
    exposing
        ( Project
        , ProjectCost
        , Model
        , Msg(..)
        )


allProjects : List Project
allProjects =
    [ improvedAutoClippers
    , begForMoreWire
    ]


improvedAutoClippers : Project
improvedAutoClippers =
    { id = "project1"
    , name = "Improved AutoClippers"
    , description = "Increases AutoClipper performance 25%"
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
        , MapPasteisLevel (always 1)
        ]
    , cost = Models.ProjectCost 0 750 0 0
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
        [ MapTrust (flip (-) 1)
        , MapWire (always 1)
        ]
    , cost =
        Models.ProjectCost 0 750 0 100
    }
