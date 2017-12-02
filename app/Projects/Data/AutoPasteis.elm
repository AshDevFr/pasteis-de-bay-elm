module Projects.Data.AutoPasteis exposing (allProjects)

import Projects.Model exposing (Project, ProjectCost)
import Projects.Msg exposing (Msg(..))
import Projects.Data.Utils as Utils


allProjects : List Project
allProjects =
    [ improvedAutoPasteis
    , improvedAutoPasteis2
    , improvedAutoPasteis3
    , improvedAutoPasteis4
    ]


improvedAutoPasteis : Project
improvedAutoPasteis =
    let
        projectCost =
            ProjectCost 0 750 0 0
    in
        { id = "improvedAutoPasteis"
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
            List.append (Utils.costEffectList projectCost)
                [ MapPasteisBoost (flip (*) 1.25)
                ]
        , cost = projectCost
        }


improvedAutoPasteis2 : Project
improvedAutoPasteis2 =
    let
        projectCost =
            ProjectCost 0 2500 0 0
    in
        { id = "improvedAutoPasteis2"
        , name = "Even Better AutoPasteis"
        , description = "Increases AutoPasteis performance by an additional 50%"
        , trigger =
            (\model ->
                model.projectsModule
                    |> Maybe.map (\mod -> Utils.isActivated mod "improvedAutoPasteis")
                    |> Maybe.withDefault False
            )
        , effect =
            List.append (Utils.costEffectList projectCost)
                [ MapPasteisBoost (flip (*) 1.5)
                ]
        , cost = projectCost
        }


improvedAutoPasteis3 : Project
improvedAutoPasteis3 =
    let
        projectCost =
            ProjectCost 0 5000 0 0
    in
        { id = "improvedAutoPasteis3"
        , name = "Optimized AutoPasteis"
        , description = "Increases AutoPasteis performance by an additional 75%"
        , trigger =
            (\model ->
                model.projectsModule
                    |> Maybe.map (\mod -> Utils.isActivated mod "improvedAutoPasteis2")
                    |> Maybe.withDefault False
            )
        , effect =
            List.append (Utils.costEffectList projectCost)
                [ MapPasteisBoost (flip (*) 1.75)
                ]
        , cost = projectCost
        }


improvedAutoPasteis4 : Project
improvedAutoPasteis4 =
    let
        projectCost =
            ProjectCost 0 5000 0 0
    in
        { id = "improvedAutoPasteis4"
        , name = "Hadwiger Clip Diagrams"
        , description = "Increases AutoPasteis performance by an additional 500%"
        , trigger =
            (\model ->
                model.projectsModule
                    |> Maybe.map (\mod -> Utils.isActivated mod "moreTrust1")
                    |> Maybe.withDefault False
            )
        , effect =
            List.append (Utils.costEffectList projectCost)
                [ MapPasteisBoost (flip (*) 1.75)
                ]
        , cost = projectCost
        }
