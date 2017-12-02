module Projects.Data.Dough exposing (allProjects)

import Projects.Model exposing (Project, ProjectCost)
import Projects.Msg exposing (Msg(..))
import Projects.Data.Utils as Utils


allProjects : List Project
allProjects =
    [ autoPurchase
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
