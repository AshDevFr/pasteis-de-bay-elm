module Projects.Data.Dough exposing (allProjects)

import Projects.Model exposing (Project, ProjectCost)
import Projects.Msg exposing (Msg(..))
import Projects.Data.Utils as Utils


allProjects : List Project
allProjects =
    [ autoPurchase
    , doughSupply1
    , doughSupply2
    , doughSupply3
    , doughSupply4
    , doughSupply5
    ]


autoPurchase : Project
autoPurchase =
    let
        projectCost =
            ProjectCost 0 7000 0 0
    in
        { id = "autoPurchase"
        , name = "Auto buyer"
        , description = "Automatically purchases dough when you run out"
        , trigger =
            (\model ->
                model.manufacturingModule.doughBasePrice >= 16
            )
        , effect =
            List.append (Utils.costEffectList projectCost)
                [ MapDoughAutoBuy
                ]
        , cost = projectCost
        }


doughSupply1 : Project
doughSupply1 =
    let
        projectCost =
            ProjectCost 0 1750 0 0
    in
        { id = "doughSupply1"
        , name = "Improved Dough creation"
        , description = "50% more dough supply from every batch"
        , trigger =
            (\model ->
                model.manufacturingModule.doughBasePrice >= 15
            )
        , effect =
            List.append (Utils.costEffectList projectCost)
                [ MapDoughSupply (flip (*) 1.5)
                ]
        , cost = projectCost
        }


doughSupply2 : Project
doughSupply2 =
    let
        projectCost =
            ProjectCost 0 3500 0 0
    in
        { id = "doughSupply2"
        , name = "Optimized Dough creation"
        , description = "75% more wire supply from every batch"
        , trigger =
            (\model ->
                model.projectsModule
                    |> Maybe.map (\mod -> Utils.isActivated mod "doughSupply1")
                    |> Maybe.withDefault False
            )
        , effect =
            List.append (Utils.costEffectList projectCost)
                [ MapDoughSupply (flip (*) 1.75)
                ]
        , cost = projectCost
        }


doughSupply3 : Project
doughSupply3 =
    let
        projectCost =
            ProjectCost 0 7500 0 0
    in
        { id = "doughSupply3"
        , name = "Microlattice Shapecasting"
        , description = "100% more wire supply from every batch"
        , trigger =
            (\model ->
                model.projectsModule
                    |> Maybe.map (\mod -> Utils.isActivated mod "doughSupply2")
                    |> Maybe.withDefault False
            )
        , effect =
            List.append (Utils.costEffectList projectCost)
                [ MapDoughSupply (flip (*) 2)
                ]
        , cost = projectCost
        }


doughSupply4 : Project
doughSupply4 =
    let
        projectCost =
            ProjectCost 0 12000 0 0
    in
        { id = "doughSupply4"
        , name = "Spectral Froth Annealment"
        , description = "200% more wire supply from every batch"
        , trigger =
            (\model ->
                model.projectsModule
                    |> Maybe.map (\mod -> Utils.isActivated mod "doughSupply3")
                    |> Maybe.withDefault False
            )
        , effect =
            List.append (Utils.costEffectList projectCost)
                [ MapDoughSupply (flip (*) 3)
                ]
        , cost = projectCost
        }


doughSupply5 : Project
doughSupply5 =
    let
        projectCost =
            ProjectCost 0 12000 0 0
    in
        { id = "doughSupply5"
        , name = "Quantum Foam Annealment"
        , description = "1,000% more wire supply from every batch"
        , trigger =
            (\model ->
                model.manufacturingModule.doughBasePrice >= 125
            )
        , effect =
            List.append (Utils.costEffectList projectCost)
                [ MapDoughSupply (flip (*) 11)
                ]
        , cost = projectCost
        }
