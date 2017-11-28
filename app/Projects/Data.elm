module Projects.Data
    exposing
        ( allProjects
        )

import Dict as Dict
import Projects.Module.ProjectsModule exposing (ProjectsModule)
import Projects.Model exposing (Project, ProjectCost)
import Projects.Msg exposing (Msg(..))


isActivated : ProjectsModule -> String -> Bool
isActivated projectsModule projectId =
    (Dict.get projectId projectsModule.projectsActivated == Just True)


costEffectList : ProjectCost -> List Msg
costEffectList projectCost =
    let
        fundsEffect =
            costEffect projectCost.funds (MapFunds (flip (-) projectCost.funds))

        operationsEffect =
            costEffect projectCost.operations (MapOperations (flip (-) projectCost.operations))

        creativityEffect =
            costEffect projectCost.creativity (MapCreativity (flip (-) projectCost.creativity))

        trustEffect =
            costEffect projectCost.trust (MapTrust (flip (-) projectCost.trust))
    in
        List.filter (\msg -> msg /= NoEffect)
            [ fundsEffect
            , operationsEffect
            , creativityEffect
            , trustEffect
            ]


costEffect : number -> Msg -> Msg
costEffect number msg =
    case number of
        0 ->
            NoEffect

        _ ->
            msg


allProjects : List Project
allProjects =
    [ improvedAutoPasteis
    , creativity
    , begForMoreWire
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
            List.append (costEffectList projectCost)
                [ MapPasteisBoost (flip (+) 0.25)
                ]
        , cost = projectCost
        }


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
            List.append (costEffectList projectCost)
                [ MapEnableCreativity
                ]
        , cost = projectCost
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
        ]
    , cost =
        ProjectCost 0 0 0 1
    }
