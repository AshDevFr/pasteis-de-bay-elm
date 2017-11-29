module Projects.Update
    exposing
        ( update
        , buy
        , runTrigger
        )

import Dict as Dict
import Main.Model exposing (Model)
import Projects.Init as Projects exposing (..)
import Projects.Msg as Projects exposing (..)
import Projects.Model exposing (Project, ProjectCost)
import Projects.Module.ProjectsModule exposing (ProjectsModule)


update : Projects.Msg -> ProjectsModule -> ( ProjectsModule, Cmd msg )
update msg projectsModule =
    ( projectsModule, Cmd.none )


buy : Model -> ProjectCost -> Result String Model
buy model cost =
    let
        enoughFunds =
            model.businessModule.funds >= cost.funds

        computingModule =
            Maybe.withDefault
                { trust = 0
                , trustLevel = 0
                , processors = 0
                , memory = 0
                , memoryLimit = 0
                , operations = 0
                , creativity = Just 0
                }
                model.computingModule

        creativity =
            Maybe.withDefault 0 computingModule.creativity

        enoughOperations =
            computingModule.operations >= cost.operations

        enoughCreativity =
            creativity >= cost.creativity

        enoughTrust =
            (computingModule.trust - (computingModule.processors + computingModule.memory)) >= cost.trust
    in
        case (enoughFunds && enoughOperations && enoughCreativity && enoughTrust) of
            False ->
                Err "Money can't buy happiness. Or lots of it."

            True ->
                Ok model


runTrigger : Model -> Model
runTrigger model =
    model.projectsModule
        |> Maybe.map
            (\mod ->
                List.foldl tryEnableProject model (inactiveList mod)
            )
        |> Maybe.withDefault model


tryEnableProject : Project -> Model -> Model
tryEnableProject project model =
    model.projectsModule
        |> Maybe.map
            (\mod ->
                case project.trigger model of
                    False ->
                        model

                    True ->
                        let
                            projectsEnabled =
                                Dict.insert project.id True mod.projectsEnabled

                            newProjectsModule =
                                { mod
                                    | projectsEnabled = projectsEnabled
                                }
                        in
                            { model | projectsModule = Just newProjectsModule }
            )
        |> Maybe.withDefault model
