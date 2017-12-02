module Projects.Data.Marketing exposing (allProjects)

import Projects.Model exposing (Project, ProjectCost)
import Projects.Msg exposing (Msg(..))
import Projects.Data.Utils as Utils


allProjects : List Project
allProjects =
    [ marketingBoost1
    , marketingBoost2
    , marketingBoost3
    ]


marketingBoost1 : Project
marketingBoost1 =
    let
        projectCost =
            ProjectCost 0 2500 25 0
    in
        { id = "marketingBoost1"
        , name = "New Slogan"
        , description = "Improve marketing effectiveness by 50%"
        , trigger =
            (\model ->
                model.projectsModule
                    |> Maybe.map (\mod -> Utils.isActivated mod "trustBoost2")
                    |> Maybe.withDefault False
            )
        , effect =
            List.append (Utils.costEffectList projectCost)
                [ MapMarketingEffectiveness (flip (*) 1.5)
                ]
        , cost = projectCost
        }


marketingBoost2 : Project
marketingBoost2 =
    let
        projectCost =
            ProjectCost 0 4500 45 0
    in
        { id = "marketingBoost2"
        , name = "Catchy Jingle"
        , description = "Double marketing effectiveness"
        , trigger =
            (\model ->
                model.projectsModule
                    |> Maybe.map (\mod -> Utils.isActivated mod "trustBoost3")
                    |> Maybe.withDefault False
            )
        , effect =
            List.append (Utils.costEffectList projectCost)
                [ MapMarketingEffectiveness (flip (*) 2)
                ]
        , cost = projectCost
        }


marketingBoost3 : Project
marketingBoost3 =
    let
        projectCost =
            ProjectCost 0 7500 0 1
    in
        { id = "marketingBoost3"
        , name = "Hypno Harmonics"
        , description = "Use neuro-resonant frequencies to influence consumer behavior"
        , trigger =
            (\model ->
                model.projectsModule
                    |> Maybe.map (\mod -> Utils.isActivated mod "marketingBoost2")
                    |> Maybe.withDefault False
            )
        , effect =
            List.append (Utils.costEffectList projectCost)
                [ MapMarketingEffectiveness (flip (*) 5)
                ]
        , cost = projectCost
        }
