module Projects.Data.Utils
    exposing
        ( isActivated
        , costEffectList
        , costEffect
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
