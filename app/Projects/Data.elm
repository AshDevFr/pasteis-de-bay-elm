module Projects.Data
    exposing
        ( allProjects
        )

import Projects.Model exposing (Project, ProjectCost)
import Projects.Msg exposing (Msg(..))
import Projects.Data.Utils as Utils
import Projects.Data.AutoPasteis as AutoPasteis
import Projects.Data.AutoMegaPasteis as AutoMegaPasteis


allProjects : List Project
allProjects =
    List.concat
        [ [ creativity
          , begForMoreWire
          ]
        , AutoPasteis.allProjects
        , AutoMegaPasteis.allProjects
        ]


creativity : Project
creativity =
    let
        projectCost =
            ProjectCost 0 1000 0 0
    in
        { id = "creativity"
        , name = "Creativity"
        , description = "Use idle operations to generate new problems and new solutions"
        , trigger =
            (\model ->
                model.computingModule
                    |> Maybe.map (\mod -> (floor mod.operations) >= mod.memoryLimit)
                    |> Maybe.withDefault False
            )
        , effect =
            List.append (Utils.costEffectList projectCost)
                [ MapEnableCreativity
                ]
        , cost = projectCost
        }


begForMoreWire : Project
begForMoreWire =
    let
        projectCost =
            ProjectCost 0 1000 0 0
    in
        { id = "projectButton2"
        , name = "Beg for More Wire"
        , description =
            "Admit failure, ask for budget increase to cover cost of 1 spool"
        , trigger =
            (\model ->
                let
                    funds =
                        model.businessModule.funds

                    inventory =
                        model.businessModule.inventory

                    dough =
                        model.manufacturingModule.dough

                    doughCost =
                        toFloat model.manufacturingModule.doughCost
                in
                    (funds < doughCost && inventory < 1 && dough < 1)
            )
        , effect =
            List.append (Utils.costEffectList projectCost)
                [ MapDough (flip (+) 1000)
                ]
        , cost = projectCost
        }
