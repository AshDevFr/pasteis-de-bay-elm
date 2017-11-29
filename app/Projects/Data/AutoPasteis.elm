module Projects.Data.AutoPasteis exposing (allProjects)

import Projects.Module.ProjectsModule exposing (ProjectsModule)
import Projects.Model exposing (Project, ProjectCost)
import Projects.Msg exposing (Msg(..))
import Projects.Data.Utils as Utils


allProjects : List Project
allProjects =
    [ improvedAutoPasteis
    , improvedAutoPasteis2
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
        , name = "Even Better AutoClippers"
        , description = "Increases AutoClipper performance by an additional 50%"
        , trigger =
            (\model ->
                let
                    enoughOps =
                        model.computingModule
                            |> Maybe.map (\mod -> mod.operations > projectCost.operations)
                            |> Maybe.withDefault False

                    previousActivated =
                        model.projectsModule
                            |> Maybe.map (\mod -> Utils.isActivated mod "improvedAutoPasteis")
                            |> Maybe.withDefault False

                    log1 =
                        Debug.log "previousActivated" previousActivated

                    log2 =
                        Debug.log "enoughOps" enoughOps
                in
                    (previousActivated && enoughOps)
            )
        , effect =
            List.append (Utils.costEffectList projectCost)
                [ MapPasteisBoost (flip (*) 1.5)
                ]
        , cost = projectCost
        }
