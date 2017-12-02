module Projects.Data.AutoMegaPasteis exposing (allProjects)

import Projects.Model exposing (Project, ProjectCost)
import Projects.Msg exposing (Msg(..))
import Projects.Data.Utils as Utils


allProjects : List Project
allProjects =
    [ enableAutoMegaPasteis
    , improvedAutoMegaPasteis
    , improvedAutoMegaPasteis2
    , improvedAutoMegaPasteis3
    ]


enableAutoMegaPasteis : Project
enableAutoMegaPasteis =
    let
        projectCost =
            ProjectCost 0 14000 0 0
    in
        { id = "enableAutoMegaPasteis"
        , name = "Improved AutoMegaPasteis"
        , description = "Increases AutoMegaPasteis performance 25%"
        , trigger =
            (\model ->
                model.manufacturingModule.pasteisModule
                    |> Maybe.map (\mod -> mod.level)
                    |> Maybe.withDefault 0
                    |> flip (>=) 75
            )
        , effect =
            List.append (Utils.costEffectList projectCost)
                [ MapEnableMegaPasteis
                ]
        , cost = projectCost
        }


improvedAutoMegaPasteis : Project
improvedAutoMegaPasteis =
    let
        projectCost =
            ProjectCost 0 14000 0 0
    in
        { id = "improvedAutoMegaPasteis"
        , name = "Improved AutoMegaPasteis"
        , description = "Increases AutoMegaPasteis performance 25%"
        , trigger =
            (\model ->
                model.projectsModule
                    |> Maybe.map (\mod -> Utils.isActivated mod "enableAutoMegaPasteis")
                    |> Maybe.withDefault False
            )
        , effect =
            List.append (Utils.costEffectList projectCost)
                [ MapMegaPasteisBoost (flip (*) 1.25)
                ]
        , cost = projectCost
        }


improvedAutoMegaPasteis2 : Project
improvedAutoMegaPasteis2 =
    let
        projectCost =
            ProjectCost 0 17000 0 0
    in
        { id = "improvedAutoMegaPasteis2"
        , name = "Even Better AutoMegaPasteis"
        , description = "Increases AutoMegaPasteis performance by an additional 50%"
        , trigger =
            (\model ->
                model.projectsModule
                    |> Maybe.map (\mod -> Utils.isActivated mod "improvedAutoMegaPasteis")
                    |> Maybe.withDefault False
            )
        , effect =
            List.append (Utils.costEffectList projectCost)
                [ MapMegaPasteisBoost (flip (*) 1.5)
                ]
        , cost = projectCost
        }


improvedAutoMegaPasteis3 : Project
improvedAutoMegaPasteis3 =
    let
        projectCost =
            ProjectCost 0 19500 0 0
    in
        { id = "improvedAutoMegaPasteis3"
        , name = "Optimized AutoMegaPasteis"
        , description = "Increases AutoMegaPasteis performance by an additional 100%%"
        , trigger =
            (\model ->
                model.projectsModule
                    |> Maybe.map (\mod -> Utils.isActivated mod "improvedAutoMegaPasteis2")
                    |> Maybe.withDefault False
            )
        , effect =
            List.append (Utils.costEffectList projectCost)
                [ MapMegaPasteisBoost (flip (*) 2)
                ]
        , cost = projectCost
        }
